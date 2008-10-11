#!/usr/bin/perl
use strict;

use Getopt::Long;
use File::Path qw/mkpath/;

my $input;
my $instances;
my $instances_input;
my $test_dir_name = 'test';
my $testset_id;
my $result_list_url = q<http://suika.fam.cx/gate/test-results/list/>;

require JSON;
use Pod::Usage;

GetOptions (
  'help' => sub {
    pod2usage (-exitval => 0, -verbose => 2);
  },
  'idl-file-name=s' => sub {
    if ($_[1] =~ /\b([A-Za-z0-9_-]+)(?:\.[A-Za-z0-9_-]+)?$/) {
      $testset_id = $1 unless defined $testset_id;
    }
    open my $file, '<:utf8', $_[1] or die "$0: $_[1]: $!";
    local $/ = undef;
    $input = '';
    $input .= $_ while <$file>;
  },
  'instances-file-name=s' => sub {
    open my $file, '<:utf8', $_[1] or die "$0: $_[1]: $!";
    local $/ = undef;
    $instances_input = '';
    $instances_input .= $_ while <$file>;

    $instances = JSON::jsonToObj ($instances_input);
  },
  'result-list-url=s' => \$result_list_url,
  'test-dir-name=s' => \$test_dir_name,
  'testset-id=s' => \$testset_id,
) or pod2usage (-exitval => 1, -verbose => 1);
pod2usage (-exitval => 1, -verbose => 1,
           -msg => "Required argument --testset-id is not specified.\n")
      unless defined $testset_id;
pod2usage (-exitval => 1, -verbose => 1,
           -msg => "Required argument --idl-file-name is not specified.\n")
      unless defined $input;

my $testset_dir_name = $test_dir_name . '/' . $testset_id . '/';
mkpath $testset_dir_name;

my $all_tests = [];

my $crash_info = $instances->{_crash} || {};
delete $instances->{_crash} if $instances;

require Whatpm::WebIDL;
my $p = Whatpm::WebIDL::Parser->new;
my $defs = $p->parse_char_string ($input);

## The transitive closure of the "inherited by" relationship, except
## for the trivial I->I relationship.
my $inherited_by = {};
my @interface = @{$defs->child_nodes};
while (@interface) {
  my $interface = shift @interface;
  if ($interface->isa ('Whatpm::WebIDL::Interface')) {
    my @inherits = map {[split /::/, $_]->[-1]} @{$interface->inheritances};
    $inherited_by->{$_}->{$interface->node_name} = 1 for @inherits;
  } elsif ($interface->isa ('Whatpm::WebIDL::Module')) {
    unshift @interface, @{$interface->child_nodes};
  }
}
while (1) {
  ## NOTE: This is not so good with respect to the computation
  ## complexity, but it should not affect the performance so much,
  ## since the number of interfaces is not so many in general.
  my $changed;
  for my $a (keys %$inherited_by) {
    for my $b (keys %{$inherited_by->{$a}}) {
      for my $c (keys %{$inherited_by->{$b} or {}}) {
        unless ($inherited_by->{$a}->{$c}) {
          $inherited_by->{$a}->{$c} = 1;
          $changed = 1;
        }
      }
    }
  }
  last unless $changed;
}

@interface = @{$defs->child_nodes};
while (@interface) {
  my $interface = shift @interface;
  if ($interface->isa ('Whatpm::WebIDL::Interface')) {
    next if $interface->is_forward_declaration;

    my $interface_name = $interface->node_name;
    my $interface_id = generate_id ($interface_name, 1);

    my $all_instances = [];
    for my $i ($interface_name,
               keys %{$inherited_by->{$interface_name} or {}}) {
      push @$all_instances, map {[$i, $_]} @{$instances->{$i} or []};
    }

    ## Interface object
    my $has_interface_object;
    unless ($interface->has_extended_attribute ('NoInterfaceObject')) {
      $has_interface_object = 1;

      generate_test
        ($interface_id . '-interface-object-has-property',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertTrue ('$interface_name' in global, '0');\n},
         label => qq{$interface_name (interface object)});
      
      generate_test
        ($interface_id . '-interface-object-dont-delete',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertDontDelete (global, '$interface_name', '1');\n},
         depends => [$interface_id . '-interface-object-has-property'],
         label => qq{$interface_name {DontDelete}});
      
      generate_test
        ($interface_id . '-interface-object-dont-enum',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertDontEnum (global, '$interface_name', '1');\n},
         depends => [$interface_id . '-interface-object-has-property'],
         label => qq{$interface_name {DontEnum}});

      generate_test
        ($interface_id . '-interface-object-prototype',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertEquals (global.$interface_name.__proto__,
                             Object.prototype,
                             'same-as-object-prototype');\n},
         depends => ['__proto__',
                     $interface_id . '-interface-object-has-property'],
         label => qq{$interface_name.[[Prototype]]});

      ## MUST have [[Construct]] - can't be tested (though we can test
      ## its existence by invoking |new|, but it might invoke an
      ## exception and there is no reliable way to distinguish the
      ## exception from the failure to invoke [[Construct]] because of
      ## lack of it (WebIDL allows any possible implementation of
      ## [[Construct]], which implies any exception may be thrown)).

      ## Interface prototype object
      generate_test
        ($interface_id . '-interface-prototype-object-has-property',
         qq{wttAssertTrue ('prototype' in $interface_name, '0');\n},
         depends => [$interface_id . '-interface-object-has-property'],
         label => qq{$interface_name.prototype});
      
      generate_test
        ($interface_id . '-interface-prototype-object-dont-delete',
         qq{wttAssertDontDelete ($interface_name, 'prototype', '1');\n},
         depends => [$interface_id .
                     '-interface-prototype-object-has-property'],
         label => qq{$interface_name.prototype {DontDelete}});
    
      generate_test
        ($interface_id . '-interface-prototype-object-read-only',
         qq{wttAssertReadOnly ($interface_name, 'prototype', '1');\n},
         depends => [$interface_id .
                     '-interface-prototype-object-has-property'],
         label => qq{$interface_name.prototype {ReadOnly}});

      ## Interface object's [[HasInstance]]
      generate_test
        ($interface_id . '-interface-object-has-instance-non-object',
         qq{wttAssertFalse (null instanceof $interface_name, 'null');\n} .
         qq{wttAssertFalse (undefined instanceof $interface_name,
                            'undefined');\n} .
         qq{wttAssertFalse (0 instanceof $interface_name, 'number');\n} .
         qq{wttAssertFalse ("" instanceof $interface_name, 'string');\n},
         depends => [$interface_id .'-interface-object-has-property'],
         label => qq{non_object instanceof $interface_name});
          ## NOTE: WebIDL's algorithm, step 1 cases

      generate_test
        ($interface_id .
         '-interface-object-has-instance-host-object-' . $_->[1]->{id},
         qq{var v = wttGetInstance ('$_->[0]', '$_->[1]->{id}');\n} .
         qq{wttAssertTrue (v instanceof $interface_name, '1');\n},
         depends => [$interface_id . '-interface-object-has-property'],
         label => qq{${interface_name}_instance ($_->[1]->{id}) instanceof } .
             $interface_name)
          for @$all_instances;
          ## NOTE: WebIDL's algorithm, step 5 cases

      generate_test
        ($interface_id . '-interface-object-has-instance-prototype-null',
         qq{function V () {};\n} .
         qq{V.prototype = null;\n} .
         qq{var v = new V ();\n} .
         qq{wttAssertFalse (v instanceof $interface_name, 'null');\n},
         depends => [$interface_id . '-interface-object-has-property'],
         label => qq{{[[Prototype]]: null} instanceof $interface_name});
          ## NOTE: WebIDL's algorithm, step 7 cases

      generate_test
        ($interface_id . '-interface-object-has-instance-prototype-object',
         qq{function V () {};\n} .
         qq{V.prototype = $interface_name.prototype;\n} .
         qq{var v = new V ();\n} .
         qq{wttAssertTrue (v instanceof $interface_name, 'level-0');\n} .
         qq{function W () {};\n} .
         qq{W.prototype = v;\n} .
         qq{var w = new W ();\n} .
         qq{wttAssertTrue (w instanceof $interface_name, 'level-1');\n},
         depends => [$interface_id . '-interface-object-has-property',
                     $interface_id .
                     '-interface-prototype-object-has-property'],
         label => qq{Object instanceof $interface_name});
          ## NOTE: WebIDL's algorithm, step 8 cases

      generate_test
        ($interface_id .
         '-interface-object-has-instance-prototype-differeent-object',
         qq{wttAssertFalse ({} instanceof $interface_name, 'object');\n} .
         qq{wttAssertFalse ((new Date ()) instanceof $interface_name,
                            'date');\n} .
         qq{wttAssertFalse ([] instanceof $interface_name, 'array');\n},
         depends => [$interface_id . '-interface-object-has-property'],
         label => qq{builtin instanceof $interface_name});
          ## NOTE: WebIDL's algorithm, step 9 -> step 7 cases
    
      if ($interface->has_extended_attribute ('Constructor')) {
        ## Interface prototype object's constructor
        generate_test
          ($interface_id .
           '-interface-prototype-object-constructor-has-property',
           qq{wttAssertTrue ('constructor' in $interface_name.prototype,
                             '0');\n},
           depends => [$interface_id .
                       '-interface-prototype-object-has-property'],
           label => qq{$interface_name.prototype.constructor});
        
        generate_test
          ($interface_id .
           '-interface-prototype-object-constructor-dont-enum',
           qq{wttAssertDontEnum ($interface_name.prototype, 'constructor',
                                 '0');\n},
           depends => [$interface_id .
                       '-interface-prototype-object-constructor-has-property'],
           label => qq{$interface_name.prototype.constructor {DontEnum}});
      }

      ## TODO: "However, it MUST be an object that provides access to
      ## the properties corresponding to the operations and constants
      ## defined on the interfaces from which this interface
      ## inherits. Changes made to the interface prototype objects of
      ## superinterfaces MUST be reflected through this object, as
      ## with normal prototype-based single inheritance in
      ## ECMAScript. If more than one superinterface has a given
      ## property, it is implementation specific which one is
      ## accessed. "

      ## ToString
      if ($interface->has_extended_attribute ('Stringifies')) {
        generate_test
          ($interface_id .
           '-interface-prototype-object-to-string-has-property',
           qq{wttAssertTrue ('toString' in $interface_name.prototype, '1');\n},
           depends => [$interface_id .
                       '-interface-prototype-object-has-property'],
           label => qq{$interface_name.prototype.toString});

        generate_test
          ($interface_id . '-interface-prototype-object-to-string-type',
           qq{wttAssertEquals (typeof ($interface_name.prototype.toString),
                               'function', 'function');\n},
           depends => [$interface_id .
                       '-interface-prototype-object-to-string-has-property'],
           label => qq{typeof ($interface_name.prototype.toString) === Function});
        ## NOTE: There is no reliable way to distinguish a Function
        ## object from some host object that behaves as if it were a
        ## Function object, afaict.
      }
    }

    for (@{$interface->get_extended_attribute_nodes ('NamedConstructor')}) {
      my $name = $_->value;
      my $id = generate_id ($name, 1);

      generate_test
        ($interface_id . '-constructor-' . $id . '-has-property',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertTrue ('$name' in global, '0');\n},
         label => qq{$name (Constructor)});
      
      generate_test
        ($interface_id . '-constructor-' . $id . '-dont-delete',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertDontDelete (global, '$name', '1');\n},
         depends => [$interface_id . '-constructor-' . $id . '-has-property'],
         label => qq{$name {DontDelete}});
      
      generate_test
        ($interface_id . '-constructor-' . $id . '-dont-enum',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertDontEnum (global, '$name', '1');\n},
         depends => [$interface_id . '-constructor-' . $id . '-has-property'],
         label => qq{$name {DontEnum}});
      
      ## [[Construct]] must return an object implementing the
      ## interface or throw an exception - don't check for now (see
      ## note above).
    }

    my %has_method;
    for my $def (@{$interface->child_nodes}) {
      if ($def->isa ('Whatpm::WebIDL::Const')) {
        my $const_name = $def->node_name;
        my $const_value = $def->value_text;
        my $const_id = generate_id ($const_name, 2);

        if ($has_interface_object) {
          for my $aaa (['interface-object', $interface_name, []],
                       ['interface-prototype-object',
                        $interface_name.'.prototype',
                        [$interface_id .
                         '-interface-prototype-object-has-property']]) {
            generate_test
              ($interface_id . '-'.$aaa->[0].'-const-' . $const_id .
               '-has-property',
               qq{wttAssertTrue ('$const_name' in $aaa->[1], '1');\n},
               depends => $aaa->[2],
               label => qq{$aaa->[1].$const_name});
            
            generate_test
              ($interface_id . '-'.$aaa->[0].'-const-' . $const_id .
               '-dont-delete',
               qq{wttAssertDontDelete ($aaa->[1], '$const_name', '1');\n},
               depends => [$interface_id . '-'.$aaa->[0].'-const-'.
                           $const_id .'-has-property'],
               label => qq{$aaa->[1].$const_name {DontDelete}});
            
            generate_test
              ($interface_id . '-'.$aaa->[0].'-const-' . $const_id .
               '-read-only',
               qq{wttAssertReadOnly ($aaa->[1], '$const_name', '1');\n},
               depends => [$interface_id . '-'.$aaa->[0].'-const-'.
                           $const_id .'-has-property'],
               label => qq{$aaa->[1].$const_name {ReadOnly}});

            generate_test
              ($interface_id . '-'.$aaa->[0].'-const-' . $const_id . '-value',
               qq{wttAssertEquals ($aaa->[1].$const_name, $const_value,
                                   '1');\n},
               depends => [$interface_id . '-'.$aaa->[0].'-const-'.
                           $const_id .'-has-property'],
               label => qq{$aaa->[1].$const_name value});
          }
        }
      } elsif ($def->isa ('Whatpm::WebIDL::Operation')) {
        my $method_name = $def->node_name;
        my $method_id = generate_id ($method_name);

        next if $has_method{$method_name};
        $has_method{$method_name} = 1;

        generate_test
          ($interface_id . '-interface-prototype-object-method-' . $method_id .
           '-has-property',
           qq{wttAssertTrue ('$method_name' in $interface_name.prototype,
                             '1');\n},
           depends => [$interface_id .
                       '-interface-prototype-object-has-property'],
           label => qq{$interface_name.prototype.$method_name});
        
        generate_test
          ($interface_id . '-interface-prototype-object-method-' . $method_id .
           '-dont-enum',
           qq{wttAssertDontEnum ($interface_name.prototype, '$method_name',
                                 '1');\n},
           depends => [$interface_id . '-interface-prototype-object-method-'.
                       $method_id .'-has-property'],
           label => qq{$interface_name.prototype.$method_name {DontEnum}});
        
        ## TODO: If there is multiple definitions for the same
        ## identifier, test whether a TypeError is thrown in case
        ## arguments are less than the minimum number of arguments for
        ## the operation.
      } elsif ($def->isa ('Whatpm::WebIDL::Attribute')) {
        my $attr_name = $def->node_name;
        my $attr_id = generate_id ($attr_name);

        for my $i (@$all_instances) {
          generate_test
            ($interface_id . '-instance-' . $i->[1]->{id} .
             '-attr-' . $attr_id . '-has-property',
             qq{var v = wttGetInstance ('$i->[0]', '$i->[1]->{id}');\n} .
             qq{wttAssertTrue ('$attr_name' in v, '1');\n},
             label => qq{${interface_name}_instance ($i->[1]->{id}).$attr_name});
          
          generate_test
            ($interface_id . '-instance-' . $i->[1]->{id} .
             '-attr-' . $attr_id . '-dont-delete',
             qq{var v = wttGetInstance ('$i->[0]', '$i->[1]->{id}');\n} .
             qq{wttAssertDontDelete (v, '$attr_name', '1');\n},
             depends => [$interface_id . '-instance-' . $i->[1]->{id} .
                         '-attr-' . $attr_id . '-has-property'],
             label => qq{${interface_name}_instance ($i->[1]->{id}).$attr_name {DontDelete}});
          
          if ($def->readonly and
              not $def->has_extended_attribute ('PutForwards')) {
            generate_test
              ($interface_id . '-instance-' . $i->[1]->{id} .
               '-attr-' . $attr_id . '-read-only',
               qq{var v = wttGetInstance ('$i->[0]', '$i->[1]->{id}');\n} .
               qq{wttAssertReadOnly (v, '$attr_name', '1');\n},
               depends => [$interface_id . '-instance-' . $i->[1]->{id} .
                           '-attr-' . $attr_id . '-has-property'],
               label => qq{${interface_name}_instance ($i->[1]->{id}).$attr_name {ReadOnly}});
          }
          
          ## TODO: "Changes made to the interface prototype objects of
          ## interfaces implemented by the host object MUST be
          ## reflected through this object."
        }
      }

      ## TODO: IndexGetter/NameGetter/IndexSetter/NameSetter
    }
  } elsif ($interface->isa ('Whatpm::WebIDL::Exception')) {
    my $interface_name = $interface->node_name;
    my $interface_id = generate_id ($interface_name, 1);

    my $all_instances = [];
    push @$all_instances,
        map {[$interface_name, $_]} @{$instances->{$interface_name} or []};
    
    ## Exception interface object
    my $has_interface_object;
    unless ($interface->has_extended_attribute ('NoInterfaceObject')) {
      $has_interface_object = 1;

      generate_test
        ($interface_id . '-interface-object-has-property',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertTrue ('$interface_name' in global, '0');\n},
         label => qq{$interface_name (exception interface object)});
      
      generate_test
        ($interface_id . '-interface-object-dont-delete',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertDontDelete (global, '$interface_name', '1');\n},
         depends => [$interface_id . '-interface-object-has-property'],
         label => qq{$interface_name {DontDelete}});
      
      generate_test
        ($interface_id . '-interface-object-dont-enum',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertDontEnum (global, '$interface_name', '1');\n},
         depends => [$interface_id . '-interface-object-has-property'],
         label => qq{$interface_name {DontEnum}});

      generate_test
        ($interface_id . '-interface-object-prototype',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertEquals (global.$interface_name.__proto__,
                             Object.prototype,
                             'same-as-object-prototype');\n},
         depends => ['__proto__',
                     $interface_id . '-interface-object-has-property'],
         label => qq{$interface_name.[[Prototype]]});

      ## Exception interface prototype object
      generate_test
        ($interface_id . '-interface-prototype-object-has-property',
         qq{wttAssertTrue ('prototype' in $interface_name, '0');\n},
         depends => [$interface_id . '-interface-object-has-property'],
         label => qq{$interface_name.prototype});
      
      generate_test
        ($interface_id . '-interface-prototype-object-dont-delete',
         qq{wttAssertDontDelete ($interface_name, 'prototype', '1');\n},
         depends => [$interface_id .
                     '-interface-prototype-object-has-property'],
         label => qq{$interface_name.prototype {DontDelete}});
    
      generate_test
        ($interface_id . '-interface-prototype-object-read-only',
         qq{wttAssertReadOnly ($interface_name, 'prototype', '1');\n},
         depends => [$interface_id .
                     '-interface-prototype-object-has-property'],
         label => qq{$interface_name.prototype {ReadOnly}});

      ## Exception interface object's [[HasInstance]]
      generate_test
        ($interface_id . '-interface-object-has-instance-non-object',
         qq{wttAssertFalse (null instanceof $interface_name, 'null');\n} .
         qq{wttAssertFalse (undefined instanceof $interface_name,
                            'undefined');\n} .
         qq{wttAssertFalse (0 instanceof $interface_name, 'number');\n} .
         qq{wttAssertFalse ("" instanceof $interface_name, 'string');\n},
         depends => [$interface_id .'-interface-object-has-property'],
         label => qq{non_object instanceof $interface_name});
          ## NOTE: WebIDL's algorithm, step 1 cases

      generate_test
        ($interface_id .
         '-interface-object-has-instance-host-object-' . $_->[1]->{id},
         qq{var v = wttGetInstance ('$_->[0]', '$_->[1]->{id}');\n} .
         qq{wttAssertTrue (v instanceof $interface_name, '1');\n},
         depends => [$interface_id . '-interface-object-has-property'],
         label => qq{${interface_name}_instance ($_->[1]->{id}) instanceof } .
             $interface_name)
          for @$all_instances;
          ## NOTE: WebIDL's algorithm, step 2 cases

      generate_test
        ($interface_id . '-interface-object-has-instance-prototype-null',
         qq{function V () {};\n} .
         qq{V.prototype = null;\n} .
         qq{var v = new V ();\n} .
         qq{wttAssertFalse (v instanceof $interface_name, 'null');\n},
         depends => [$interface_id . '-interface-object-has-property'],
         label => qq{{[[Prototype]]: null} instanceof $interface_name});
          ## NOTE: WebIDL's algorithm, step 3 cases

      generate_test
        ($interface_id . '-interface-object-has-instance-prototype-object',
         qq{function V () {};\n} .
         qq{V.prototype = $interface_name.prototype;\n} .
         qq{var v = new V ();\n} .
         qq{wttAssertFalse (v instanceof $interface_name, 'level-0');\n} .
         qq{function W () {};\n} .
         qq{W.prototype = v;\n} .
         qq{var w = new W ();\n} .
         qq{wttAssertTrue (w instanceof $interface_name, 'level-1');\n},
         depends => [$interface_id . '-interface-object-has-property',
                     $interface_id .
                     '-interface-prototype-object-has-property'],
         label => qq{Object instanceof $interface_name});
          ## NOTE: WebIDL's algorithm, step 3 cases

      generate_test
        ($interface_id .
         '-interface-object-has-instance-prototype-differeent-object',
         qq{wttAssertFalse ({} instanceof $interface_name, 'object');\n} .
         qq{wttAssertFalse ((new Date ()) instanceof $interface_name,
                            'date');\n} .
         qq{wttAssertFalse ([] instanceof $interface_name, 'array');\n},
         depends => [$interface_id . '-interface-object-has-property'],
         label => qq{builtin instanceof $interface_name});
          ## NOTE: WebIDL's algorithm, step 3 cases
    }

    if ($has_interface_object) {
      my $mod = $interface->parent_node;
      if ($mod and $mod->isa ('Whatpm::WebIDL::Module')) {
        my $has_consts;
        for (@{$mod->get_extended_attribute_nodes ('ExceptionConsts')}) {
          if ($_->value eq $interface_name) {
            $has_consts = 1;
            last;
          }
        }
        
        if ($has_consts) {
          for my $def (@{$mod->child_nodes}) {
            if ($def->isa ('Whatpm::WebIDL::Const')) {
              my $const_name = $def->node_name;
              my $const_value = $def->value_text;
              my $const_id = generate_id ($const_name, 2);
              
              for my $aaa (['interface-object', $interface_name, []],
                           ['interface-prototype-object',
                            $interface_name.'.prototype',
                            [$interface_id .
                             '-interface-prototype-object-has-property']]) {
                generate_test
                  ($interface_id . '-'.$aaa->[0].'-const-' . $const_id .
                   '-has-property',
                   qq{wttAssertTrue ('$const_name' in $aaa->[1], '1');\n},
                   depends => $aaa->[2],
                   label => qq{$aaa->[1].$const_name});
                
                generate_test
                  ($interface_id . '-'.$aaa->[0].'-const-' . $const_id .
                   '-dont-delete',
                   qq{wttAssertDontDelete ($aaa->[1], '$const_name', '1');\n},
                   depends => [$interface_id . '-'.$aaa->[0].'-const-'.
                               $const_id .'-has-property'],
                   label => qq{$aaa->[1].$const_name {DontDelete}});
                
                generate_test
                  ($interface_id . '-'.$aaa->[0].'-const-' . $const_id .
                   '-read-only',
                   qq{wttAssertReadOnly ($aaa->[1], '$const_name', '1');\n},
                   depends => [$interface_id . '-'.$aaa->[0].'-const-'.
                               $const_id .'-has-property'],
                   label => qq{$aaa->[1].$const_name {ReadOnly}});
                
                generate_test
                  ($interface_id . '-'.$aaa->[0].'-const-' . $const_id .
                   '-value',
                   qq{wttAssertEquals ($aaa->[1].$const_name, $const_value,
                                       '1');\n},
                   depends => [$interface_id . '-'.$aaa->[0].'-const-'.
                               $const_id .'-has-property'],
                   label => qq{$aaa->[1].$const_name value});
              }
            }
          }
        }
      }
    }

    for my $def (@{$interface->child_nodes}) {
      if ($def->isa ('Whatpm::WebIDL::ExceptionMember')) {
        my $attr_name = $def->node_name;
        my $attr_id = generate_id ($attr_name);

        for my $i (@$all_instances) {
          generate_test
            ($interface_id . '-instance-' . $i->[1]->{id} .
             '-attr-' . $attr_id . '-has-property',
             qq{var v = wttGetInstance ('$i->[0]', '$i->[1]->{id}');\n} .
             qq{wttAssertTrue ('$attr_name' in v, '1');\n},
             label => qq{${interface_name}_instance ($i->[1]->{id}).$attr_name});
        
          generate_test
            ($interface_id . '-instance-' . $i->[1]->{id} .
             '-attr-' . $attr_id . '-dont-delete',
             qq{var v = wttGetInstance ('$i->[0]', '$i->[1]->{id}');\n} .
             qq{wttAssertDontDelete (v, '$attr_name', '1');\n},
             depends => [$interface_id . '-instance-' . $i->[1]->{id} .
                         '-attr-' . $attr_id . '-has-property'],
             label => qq{${interface_name}_instance ($i->[1]->{id}).$attr_name {DontDelete}});
        }
      }
    }

    for my $i (@$all_instances) {
      generate_test
        ($interface_id . '-instance-' . $i->[1]->{id} . '-prototype',
         qq{var v = wttGetInstance ('$i->[0]', '$i->[1]->{id}');\n} .
         qq{wttAssertEquals (v.__proto__, $interface_name.prototype,
                             'same-as-interface-prototype');\n},
         depends => ['__proto__',
                     $interface_id .
                     '-interface-prototype-object-has-property'],
         label => qq{${interface_name}_instance ($i->[1]->{id}).[[Prototype]]});
    }
  } elsif ($interface->isa ('Whatpm::WebIDL::Module')) {
    unshift @interface, @{$interface->child_nodes};
  }
}

generate_support_files ();

sub htescape ($) {
  my $s = shift;
  $s =~ s/&/&amp;/g;
  $s =~ s/</&lt;/g;
  $s =~ s/"/&quot;/g;
  return $s;
} # htescape

sub generate_id ($$) {
  my $s = shift;
  if ($_[0] == 2) {
    $s =~ tr/A-Z_/a-z-/;
    return $s;
  } else {
    $s =~ s/([A-Z]+)$/-@{[lc $1]}/;
    $s =~ s/([A-Z]+)([A-Z])/-@{[lc $1]}-@{[lc $2]}/g;
    $s =~ s/([A-Z])/-@{[lc $1]}/g;
    $s =~ s/^-// if $_[0];
  }
  return $s;
} # generate_id

sub generate_test ($$;%) {
  my ($test_id, $test_code, %opt) = @_;

  my $test_file_name = $testset_dir_name . $test_id . '.html';
  push @$all_tests, {depends => $opt{depends},
                     id => $test_id,
                     fileName => $test_id . '.html',
                     label => $opt{label}};

  open my $test_file, '>:utf8', $test_file_name
      or die "$0: $test_file_name: $!";
  print $test_file q[<!DOCTYPE HTML><title>];
  print $test_file htescape ($test_id);
  print $test_file qq[</title><script src=wtt.js></script>\n];
  print $test_file qq[<p id=result class=FAIL>FAIL (noscript)</p>\n\n];
  print $test_file qq[<script>
var globalId = '$test_id';
wttSetStatus ('FAIL', 'script')

try {

wttCheckCrash (globalId);

$test_code

wttOk ();

} catch (e) {
  if (e instanceof WttFail) {
    //
  } else if (e instanceof WttSkip) {
    wttSetStatus ('SKIPPED', e.message);
  } else {
    throw e;
  }
}

</script>];
} # generate_test

sub generate_support_files () {
  {
    my $path = $testset_dir_name . '.htaccess';
    open my $file, '>:utf8', $path or die "$0: $path: $!";
    print $file qq[AddType text/html .html\n];
    print $file qq[AddCharset utf-8 .html\n];
    print $file qq[AddType text/javascript .js\n];
    print $file qq[AddCharset utf-8 .js\n];
  }

  {
    my $path = $testset_dir_name . 'all.html';
    open my $file, '>:utf8', $path or die "$0: $path: $!";
    print $file qq[<!DOCTYPE HTML>
<title>@{[htescape ($testset_id)]}</title>
<body>
<p id=status>Not executed, since scripting is not enabled.</p>
<p><span id=passed>0</span> passed,
<span id=failed>0</span> failed,
<span id=skipped>0</span> skipped.</p>

<form method=post accept-charset=utf-8></form>

<p><a href="@{[htescape ($result_list_url.$testset_id)]}/all">See
result of other browsers</a>

<p>Failed tests:
<ul id=failed-list></ul>

<p>Skipped tests:
<ul id=skipped-list></ul>

<iframe style="border-width: 0; width: 0; height: 0"></iframe>
<script>
  var tests = @{[JSON::objToJson ($all_tests)]};
  var testsLength = tests.length;
</script>
<script>
  document.getElementById ('status').firstChild.data = 'Executing...';

  var iframe = document.getElementsByTagName ('iframe')[0];
  var form = document.forms[0];

  var testResults = {};
  var passedTestsNumber = 0;
  var failedTestsNumber = 0;
  var skippedTestsNumber = 0;
  var currentTest;

  if (document.all && !window.opera) {
    iframe.onreadystatechange = function () {
      if (this.readyState == 'complete') {
        getTestResult ();
        while (true) {
          if (nextTest ()) break;
        }
      }
    }
  } else {
    iframe.onload = function () {
      getTestResult ();
      while (true) {
        if (nextTest ()) break;
      }
    }
  }

  while (true) {
    if (nextTest ()) break;
  }

  function getTestResult () {
    if (!currentTest) return;
    
    var r = iframe.contentWindow.document.getElementById ('result');
    if (r.className == 'PASS') {
      document.getElementById ('passed').firstChild.data = ++passedTestsNumber;
      testResults[currentTest.id] = true;
    } else {
      var idPrefix = r.className == 'SKIPPED' ? 'skipped' : 'failed';
      document.getElementById (idPrefix).firstChild.data
          = (r.className == 'SKIPPED' ?
                 ++skippedTestsNumber : ++failedTestsNumber);
      var li = document.createElement ('li');
      li.innerHTML = '<a>xxxx</a>: <span>xxxx</span>';
      li.firstChild.href = currentTest.fileName;
      li.firstChild.title = currentTest.id;
      li.firstChild.firstChild.data = currentTest.label || currentTest.id;
      li.lastChild.firstChild.data = r.firstChild.data;
      document.getElementById (idPrefix + '-list').appendChild (li);
    }

    var i = document.createElement ('p');
    i.innerHTML = '<input name=test-name type=hidden>' +
        '<input name=test-label type=hidden>' +
        '<input name=test-class type=hidden>' +
        '<input name=test-result type=hidden>';
    i.childNodes[0].value = currentTest.fileName;
    i.childNodes[1].value = currentTest.label || '';
    i.childNodes[2].value = r.className;
    i.childNodes[3].value = r.firstChild.data;
    form.appendChild (i);
  } // getTestResult

  function nextTest () {
    if (tests.length > 0) {
      document.getElementById ('status').firstChild.data
          = (testsLength - tests.length + 1) + ' of ' + testsLength;

      var nextTest = tests.shift ();

      var skipTest = false;
      if (nextTest.depends) {
        for (var i in nextTest.depends) {
          var dTestId = nextTest.depends[i];
          if (!testResults[dTestId]) {
            document.getElementById ('skipped').firstChild.data
                = ++skippedTestsNumber;
            var li = document.createElement ('li');
            li.innerHTML = '<a>xxxx</a>: skipped due to failure of <a>yy</a>';
            li.firstChild.href = nextTest.fileName;
            li.firstChild.title = nextTest.id;
            li.firstChild.firstChild.data = nextTest.label || nextTest.id;
            li.lastChild.firstChild.data = dTestId;
            document.getElementById ('skipped-list').appendChild (li);

            var i = document.createElement ('p');
            i.innerHTML = '<input name=test-name type=hidden>' +
                '<input name=test-label type=hidden>' +
                '<input name=test-class type=hidden>' +
                '<input name=test-result type=hidden>';
            i.childNodes[0].value = nextTest.fileName;
            i.childNodes[1].value = nextTest.label || '';
            i.childNodes[2].value = 'SKIPPED';
            i.childNodes[3].value = 'skipped (' + dTestId + ')';
            form.appendChild (i);

            skipTest = true;
            break;
          }
        }
      }
      if (skipTest) {
        return false;
      }

      currentTest = nextTest;
      iframe.src = nextTest.fileName;
    } else {
      iframe.onreadystatechange = null;
      iframe.onload = null;
      document.getElementById ('status').firstChild.data = 'Done';

      // Submission form
      form.action = '$result_list_url$testset_id';

      var i = document.createElement ('p');
      i.innerHTML = '<input name=env-name type=hidden value="">' +
          '<input type=submit value="Submit this result">';
      i.firstChild.value = navigator.userAgent;
      form.appendChild (i);
    }

    return true;
  } // nextTest
</script>
];
  }
  
  {
    my $path = $testset_dir_name . 'wtt.js';
    open my $file, '>:utf8', $path or die "$0: $path: $!";
    print $file q[
function WttFail () {
  //
} // WttFail

function WttSkip (message) {
  this.message = message;
} // WttSkip

function wttGetGlobal () {
  return window;
} // wttGetGlobal

function wttAssertTrue (condition, localId) {
  if (!condition) {
    wttSetStatus ('FAIL', localId + ' false (true expected)');
    throw new WttFail ();
  }

  // condition is true.
} // wttAssertTrue


function wttAssertFalse (condition, localId) {
  if (condition) {
    wttSetStatus ('FAIL', localId + ' true (false expected)');
    throw new WttFail ();
  }

  // condition is true.
} // wttAssertFalse

function wttAssertEquals (actual, expected, localId) {
  if (actual !== expected) {
    wttSetStatus ('FAIL', localId + ' got ' + dumpValue (actual) +
                  ' where ' + dumpValue (expected) + ' is expected');
    throw new WttFail ();
  }
  
  // actual === expected
} // wttAssertEquals

function wttAssertDontEnum (object, propName, localId) {
  for (var n in object) {
    if (n === propName) {
      wttSetStatus ('FAIL', localId + ' (DontEnum expected)');
      throw new WttFail ();
    }
  }
  
  // object[propName] is {DontEnum}.
} // wttAssertDontEnum

function wttAssertDontDelete (object, propName, localId) {
  var propValue = object[propName];
  
  if (!delete object[propName]) {
    wttSetStatus ('FAIL', localId + ' (delete returns true)');
    throw new WttFail ();
    // According to ECMA 262, [[Delete]] returns false if DontDelete is set.
  }

  if (!(propName in object)) {
    wttSetStatus ('FAIL', localId + ' (delete does delete the property)');
    throw new WttFail ();
  }

  if (object[propName] !== propValue) {
    wttSetStatus ('FAIL', localId + ' (delete change the value to ' +
                  dumpValue (object[propName]) + ' where ' +
                  dumpValue (propValue) + ' is expected)');
    throw new WttFail ();
    
    /* The WebIDL specification does not change semantics of the
       [[Delete]] internal method from the ECMA 262 3rd edition.
       In ECMA 262, [[Delete]] does not do anything except for returning
       a |false| value in case the {DontDelete} attribute is set to the
       attribute.  Therefore, the property value must not be changed
       before and after the |delete| operation, which just invokes the
       [[Delete]] method.
    */
  }
} // wttAssertDontDelete

function wttAssertReadOnly (object, propName, localId) {
  /*
    Note that this function returns a wrong result when the [[Put]]
    or [[CanPut]] method of /object/ is replaced by another steps
    from those defined in ECMA 262.  According to WebIDL spec,
    objects conforming to that specification does not modify those
    methods unless explicitly defined (e.g. for objects with [NamedSetter]).
  */

  var propValue = object[propName];

  try {
    object[propName] = 'abcdefg';
  } catch (e) {
    /* 
      According to the [[Put]] algorithms of ECMA 262 and WebIDL, assigning
      a value to a read-only property should not raise an exception.
      However, since testing the behavior of [[Put]] is not the purpose of
      this test, we catch any exception thrown by the assignment.
      Note that it might also catch any exception thrown by non-standard
      setter extension, if any.
    */
  }
  if (object[propName] === 'abcdefg') {
    wttSetStatus ('FAIL', localId + ' (value changed)');
    throw new WttFail ();
  }
  
  if (object[propName] !== propValue) {
    wttSetStatus ('FAIL', localId + ' (putting changes value from ' +
                  dumpValue (propValue) + ' to ' +
                  dumpValue (object[propName]) + ')');
    throw new WttFail ();
  }

  // The property seems read only.
} // wttAssertReadOnly

function wttOk () {
  wttSetStatus ('PASS');
} // wttOk

function wttSetStatus (s, t) {
  var result = document.getElementById ('result');
  result.firstChild.data
      = s + ' (' + globalId + (t != null ? '-' + t : '') + ')';
  result.className = s;
} // wttSetStatus

function dumpValue (v) {
  return '"' + v + '", type ' + typeof (v);
} // dumpValue
];

    print $file qq[

var wttInstanceInfo = $instances_input;
function wttGetInstance (interface, id) {
  var giCodes = wttInstanceInfo[interface];
  if (!giCodes) {
    wttSetStatus ('FAIL',
                  'broken testcase - no code for ' + interface + ' ' + id);
    throw new WttFail ();
  }
  
  for (var i in giCodes) {
    if (giCodes[i].id == id) {
      var v;
      var message;
      try {
        v = eval (giCodes[i].code);
      } catch (e) {
        v = null;
        message = '' + e;
      }
      if (!v) {
        throw new WttSkip ('cannot obtain instance by ' + id +
                           (message ? ' (' + message + ')' : ''));
      }
      return v;
    }
  }

  if (!giCodes) {
    wttSetStatus ('FAIL',
                  'broken testcase - no code for ' + interface + ' ' + id);
    throw new WttFail ();
  }
} // wttGetInstance

var crashInfo = @{[JSON::objToJson ($crash_info)]};
function wttCheckCrash (testId) {
  var entry = crashInfo[testId];
  if (!entry) return;

  var ua = navigator.userAgent;
  for (var i = 0; i < entry.length; i++) {
    var reg = new RegExp (entry[i]);
    if (ua.match (reg)) {
      wttSetStatus ('FAIL',
                    'Skipped because it is known that this test case would crash the browser in use');
      throw new WttFail ();
    }
  }
} // wttCheckCrash

] if defined $instances_input;
  }
} # generate_support_files

__END__

=head1 NAME

wttjs.pl - WebIDL ECMAScript Binding Test Suite Generator

=head1 SYNOPSIS

  $ perl wttjs.pl \
        --idl-file-name input.idl \
        --instances-file-name input.json \
        --test-dir-name output-dir/ \
        --testset-id testname

  $ perl wttjs.pl --help

=head1 DESCRIPTION

The script C<wttjs.pl> generates a set of test cases for the
conformance of a Web browser's DOM implementation, with regard to a
set of interfaces described by a WebIDL fragment.

For more information, see the readme document
L<http://suika.fam.cx/www/webidl2tests/readme>.

=head1 ARGUMENTS

This script accepts command-line arguments in the
L<Getopt::Long|Getopt::Long> style.  Any argument can be specified at
most once.

=over 4

=item C<--help>

Show the help on the command-line arguments and exit the script.

=item C<--idl-file-name I<file-name.idl>> (B<REQUIRED>)

The name or path of the file that contains the IDL fragment that
defines a set of interfaces and exceptions to be tested against.

If this argument is not specified, if the specified file is not found,
or if the file cannot be read due to some I/O error, the script would
exit with an error message.

The file must be encoded in UTF-8 (Perl's C<utf-8> encoding).
Otherwise, the result test cases might be broken.

The file must contain a syntactically valid IDL fragment.  Any failure
to conform to the WebIDL syntax would be reported to the standard
error output.  Such errors are handled by the CSS-like
forward-compatible parsing rule as implemented by the
L<Whatpm::WebIDL|Whatpm::WebIDL> parser.

Though the content of the file don't have to be a conforming IDL
fragment, the result test cases might be broken if it is not.  It is
encouraged to check the conformance of the input IDL fragment by a
conformance checker, e.g. WebHACC
L<http://suika.fam.cx/gate/2007/html/cc/>.

=item C<--instances-file-name I<file-name.json>> (Default: No instance generation)

The name or path of the file that contains supplement information on
interfaces and exceptions defined in the IDL fragment.

If the specified file is not found, or if the file cannot be read due
to some I/O error, the script would exit with an error message.

The file must be encoded in UTF-8 (Perl's C<utf-8> encoding).
Otherwise, the file might be considered as broken, or the result test
cases might be broken.

The file must contain a JSON representation of a data structure
described in the readme document.  If the content is not valid as
JSON, then the script would exit with an error message.  If the
content does not encode the data structure specified in the readme
document, the script might exit with a Perl script execution error.
Even when no Perl error stops the script, the result test cases might
be broken.

If this argument is not specified, then it is assumed that no
additional information is available.

=item C<--test-dir-name I<path-to-dir/>> (Default: C<./tests/>)

The name or path of the directory, in which the directory for the test
files is created.

The default value that is used when this argument is not specified is
C<./tests/>.

All files generated by this script is put into the directory
C<I<test-dir-name>/I<testset-id>/>, where I<test-dir-name> is the
value specified by the C<--test-dir-name> argument and I<testset-id>
is the value specified by the C<--testset-id> argument.  If there is
no such a directory, then it is created by the script.

=item C<--testset-id I<id>> (B<REQUIRED>)

The identifier of the test suite.

If this argument is not specified, the script would exit with an error
message.

Though any value can be specified as identifier, it should be a string
consist of characters C<a>..C<z>, C<0>..C<9>, and C<-> only, with no
leading C<-> character, for filesystem safety and compatibility with
the test result summary script (see the readme document).

=back

=head1 DEPENDENCY

This script, in addition to Perl 5.8.* or later, requires the
following modules:

=over 4

=item L<JSON|JSON>

A JSON parser and serializer, which is available from CPAN.

To install the L<JSON|JSON> module from the CPAN, type:

  # perl -MCPAN -eshell
  cpan> install JSON

=item L<Whatpm::WebIDL|Whatpm::WebIDL>

A WebIDL parser and object model implementation, which is part of the
Whatpm package L<http://suika.fam.cx/www/markup/html/whatpm/readme>.

Note that L<Whatpm::WebIDL|Whatpm::WebIDL> does not depend on any
other module.

=back

=head1 SEE ALSO

Readme L<http://suika.fam.cx/www/webidl2tests/readme>.

Web IDL specification, revision 1.96 (3 September 2008 Editor's Draft)
L<http://dev.w3.org/cvsweb/~checkout~/2006/webapi/WebIDL/Overview.html?rev=1.96&content-type=text/html;%20charset=utf-8>.

L<Whatpm::WebIDL|Whatpm::WebIDL>, which is used to parse IDL
fragments.

=head1 AUTHOR

Wakaba <w@suika.fam.cx>

=head1 LICENSE

Copyright 2008 Wakaba <w@suika.fam.cx>

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut 

## $Date: 2008/10/11 08:05:37 $
