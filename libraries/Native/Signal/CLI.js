
Elm.Native.CLI = function(elm) {
  'use strict';
  elm.Native = elm.Native || {};
  if (elm.Native.CLI) return elm.Native.CLI;

  var rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });

  var Signal = Elm.Signal(elm);

  var stdin = Signal.constant('');

  /* Signal JSString */
  rl.on('line', function(line) {
    elm.notify(stdin.id, line);
  });

  var print = function(line) {
    rl.write('Print:' + line + '\n');
    return line;
  }

  /* Signal JSString -> Signal JSString */
  var stdouteffect = function(input) {
    var node = Signal.lift(print)(input);
    return node;
  }

  return elm.Native.CLI = {
    stdin : stdin,
    stdouteffect : stdouteffect
  };
};
