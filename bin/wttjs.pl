#!/usr/bin/perl
use strict;

## TODO: ...
use lib qw[/home/httpd/html/www/markup/html/whatpm
           /home/wakaba/work/manakai2/lib];

use Getopt::Long;
use File::Path qw/mkpath/;

my $input;
my $instances;
my $instances_input;
my $test_dir_name = 'test';
my $testset_id;

GetOptions (
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

    require JSON;
    $instances = JSON::jsonToObj ($instances_input);
  },
  'test-dir-name=s' => \$test_dir_name,
  'testset-id=s' => \$testset_id,
) or die; ## TODO: ...
die "## TODO: error message here (testset-id)" unless defined $testset_id;
die "## TODO: error (idl-file-name)" unless defined $input;

my $testset_dir_name = $test_dir_name . '/' . $testset_id . '/';
mkpath $testset_dir_name;

my $all_tests = [];

require Whatpm::WebIDL;
my $p = Whatpm::WebIDL::Parser->new;
my $defs = $p->parse_char_string ($input);

my @interface = @{$defs->child_nodes};
while (@interface) {
  my $interface = shift @interface;
  if ($interface->isa ('Whatpm::WebIDL::Interface')) {
    my $interface_name = $interface->node_name;
    my $interface_id = generate_id ($interface_name, 1);

    ## Interface object
    my $has_interface_object;
    unless ($interface->has_extended_attribute ('NoInterfaceObject')) {
      $has_interface_object = 1;

      generate_test
        ($interface_id . '-interface-object-has-property',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertTrue ('$interface_name' in global, '0');\n});
      
      generate_test
        ($interface_id . '-interface-object-dont-delete',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertDontDelete (global, '$interface_name', '1');\n},
         depends => [$interface_id . '-interface-object-has-property']);
      
      generate_test
        ($interface_id . '-interface-object-dont-enum',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertDontEnum (global, '$interface_name', '1');\n},
         depends => [$interface_id . '-interface-object-has-property']);

      generate_test
        ($interface_id . '-interface-object-prototype',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertEquals (global.$interface_name.__proto__,
                             Object.prototype,
                             'same-as-object-prototype');\n},
         depends => ['__proto__',
                     $interface_id . '-interface-object-has-property']);

      ## MUST have [[Construct]] - can't be tested (though we can test
      ## its existence by invoking |new|, but it might invoke an
      ## exception and there is no reliable way to distinguish the
      ## exception from the failure to invoke [[Construct]] because of
      ## lack of it (WebIDL allows any possible implementation of
      ## [[Construct]], which implies any exception may be thrown)).

      ## Instance object's [[HasInstance]]
      generate_test
        ($interface_id . '-interface-object-has-instance-non-object',
         qq{wttAssertFalse (null instanceof $interface_name, 'null');\n} .
         qq{wttAssertFalse (undefined instanceof $interface_name,
                            'undefined');\n} .
         qq{wttAssertFalse (0 instanceof $interface_name, 'number');\n} .
         qq{wttAssertFalse ("" instanceof $interface_name, 'string');\n},
         depends => [$interface_id .'-interface-object-has-property']);
          ## NOTE: WebIDL's algorithm, step 1 cases

      generate_test
        ($interface_id .
         '-interface-object-has-instance-host-object-' . $_->{id},
         qq{var v = wttGetInstance ('$interface_name', '$_->{id}');\n} .
         qq{wttAssertTrue (v instanceof $interface_name, '1');\n},
         depends => [$interface_id . '-interface-object-has-property',
                     $interface_id .
                     '-interface-prototype-object-has-property'])
          for @{$instances->{$interface_name} or []};
          ## NOTE: WebIDL's algorithm, step 5 cases

      generate_test
        ($interface_id . '-interface-object-has-instance-prototype-null',
         qq{function V () {};\n} .
         qq{V.prototype = null;\n} .
         qq{var v = new V ();\n} .
         qq{wttAssertFalse (v instanceof $interface_name, 'null');\n},
         depends => [$interface_id . '-interface-object-has-property',
                     $interface_id .
                     '-interface-prototype-object-has-property']);
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
                     '-interface-prototype-object-has-property']);
          ## NOTE: WebIDL's algorithm, step 8 cases

      generate_test
        ($interface_id .
         '-interface-object-has-instance-prototype-differeent-object',
         qq{wttAssertFalse ({} instanceof $interface_name, 'object');\n} .
         qq{wttAssertFalse ((new Date ()) instanceof $interface_name,
                            'date');\n} .
         qq{wttAssertFalse ([] instanceof $interface_name, 'array');\n},
         depends => [$interface_id . '-interface-object-has-property',
                     $interface_id .
                     '-interface-prototype-object-has-property']);
          ## NOTE: WebIDL's algorithm, step 9 -> step 7 cases

      ## Interface prototype object
      generate_test
        ($interface_id . '-interface-prototype-object-has-property',
         qq{wttAssertTrue ('prototype' in $interface_name, '0');\n},
         depends => [$interface_id . '-interface-object-has-property']);
      
      generate_test
        ($interface_id . '-interface-prototype-object-dont-delete',
         qq{wttAssertDontDelete ($interface_name, 'prototype', '1');\n},
         depends => [$interface_id .
                     '-interface-prototype-object-has-property']);
      
      generate_test
        ($interface_id . '-interface-prototype-object-read-only',
         qq{wttAssertReadOnly ($interface_name, 'prototype', '1');\n},
         depends => [$interface_id .
                     '-interface-prototype-object-has-property']);

      if ($interface->has_extended_attribute ('Constructor')) {
        ## Interface prototype object's constructor
        generate_test
          ($interface_id .
           '-interface-prototype-object-constructor-has-property',
           qq{wttAssertTrue ('constructor' in $interface_name.prototype,
                             '0');\n},
           depends => [$interface_id .
                       '-interface-prototype-object-has-property']);
        
        generate_test
          ($interface_id .
           '-interface-prototype-object-constructor-dont-enum',
           qq{wttAssertDontEnum ($interface_name.prototype, 'constructor',
                                 '0');\n},
           depends => [$interface_id .
                       '-interface-prototype-object-constructor-has-property']);
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
    }

    for (@{$interface->get_extended_attribute_nodes ('NamedConstructor')}) {
      my $name = $_->value;
      my $id = generate_id ($name, 1);

      generate_test
        ($interface_id . '-constructor-' . $id . '-has-property',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertTrue ('$name' in global, '0');\n});
      
      generate_test
        ($interface_id . '-constructor-' . $id . '-dont-delete',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertDontDelete (global, '$name', '1');\n},
         depends => [$interface_id . '-constructor-' . $id . '-has-property']);
      
      generate_test
        ($interface_id . '-constructor-' . $id . '-dont-enum',
         qq{var global = wttGetGlobal ();\n} .
         qq{wttAssertDontEnum (global, '$name', '1');\n},
         depends => [$interface_id . '-constructor-' . $id . '-has-property']);

      ## [[Construct]] must return an object implementing the
      ## interface or throw an exception - don't check for now (see
      ## note above).
    }

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
              ($interface_id . '-'.$aaa->[0].'-' . $const_id .
               '-has-property',
               qq{wttAssertTrue ('$const_name' in $aaa->[1], '1');\n},
               depends => $aaa->[2]);
            
            generate_test
              ($interface_id . '-'.$aaa->[0].'-' . $const_id .
               '-dont-delete',
               qq{wttAssertDontDelete ($aaa->[1], '$const_name', '1');\n},
               depends => [$interface_id . '-'.$aaa->[0].'-'.
                           $const_id .'-has-property']);
            
            generate_test
              ($interface_id . '-'.$aaa->[0].'-' . $const_id .
               '-read-only',
               qq{wttAssertReadOnly ($aaa->[1], '$const_name', '1');\n},
               depends => [$interface_id . '-'.$aaa->[0].'-'.
                           $const_id .'-has-property']);

            generate_test
              ($interface_id . '-'.$aaa->[0].'-' . $const_id . '-value',
               qq{wttAssertEquals ($aaa->[1].$const_name, $const_value,
                                   '1');\n},
               depends => [$interface_id . '-'.$aaa->[0].'-'.
                           $const_id .'-has-property']);
          }
        }
      }
    }
  } elsif ($interface->isa ('Whatpm::WebIDL::Exception')) {

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
                     fileName => $test_id . '.html'};

  open my $test_file, '>:utf8', $test_file_name
      or die "$0: $test_file_name: $!";
  print $test_file q[<!DOCTYPE HTML><title>];
  print $test_file htescape ($test_file_name);
  print $test_file qq[</title><script src=wtt.js></script>\n];
  print $test_file qq[<p id=result class=FAIL>FAIL (noscript)</p>\n\n];
  print $test_file qq[<script>
var globalId = '$test_id';
wttSetStatus ('FAIL', 'script')

try {

$test_code
wttOk ();

} catch (e) {
  if (!(e instanceof WttFail)) {
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
      document.getElementById ('failed').firstChild.data = ++failedTestsNumber;
      var li = document.createElement ('li');
      li.innerHTML = '<a>xxxx</a>: <span>xxxx</span>';
      li.firstChild.href = currentTest.fileName;
      li.firstChild.firstChild.data = currentTest.id;
      li.lastChild.firstChild.data = r.firstChild.data;
      document.getElementById ('failed-list').appendChild (li);
    }
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
            li.firstChild.firstChild.data = nextTest.id;
            li.lastChild.firstChild.data = dTestId;
            document.getElementById ('skipped-list').appendChild (li);
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
    }

    return true;
  }
</script>
];
  }
  
  {
    my $path = $testset_dir_name . 'wtt.js';
    open my $file, '>:utf8', $path or die "$0: $path: $!";
    print $file q[
function WttFail () {
} // WttFail

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

  object[propName] = 'abcdefg';
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
      var v = eval (giCodes[i].code);
      return v;
    }
  }

  if (!giCodes) {
    wttSetStatus ('FAIL',
                  'broken testcase - no code for ' + interface + ' ' + id);
    throw new WttFail ();
  }
} // wttGetInstance
] if defined $instances_input;
  }
} # generate_support_files
