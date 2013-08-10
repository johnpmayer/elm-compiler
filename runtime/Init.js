
(function() {
'use strict';

Elm.noscreen = function(module) {
    return init(undefined, undefined, module);
}

Elm.worker = function(module) {
    return init(ElmRuntime.Display.NONE, {}, module);
};

function init(display, container, module, moduleToReplace) {
  // defining state needed for an instance of the Elm RTS
  var inputs = [];

  function notify(id, v) {
      var timestep = Date.now();
      var changed = false;
      for (var i = inputs.length; i--; ) {
          // order is important here to avoid short-circuiting
          changed = inputs[i].recv(timestep, id, v) || changed;
      }
      return changed;
  }

  var listeners = [];
  function addListener(relevantInputs, domNode, eventName, func) {
      domNode.addEventListener(eventName, func);
      var listener = {
          relevantInputs: relevantInputs,
          domNode: domNode,
          eventName: eventName,
          func: func
      };
      listeners.push(listener);
  }

  // create the actual RTS. Any impure modules will attach themselves to this
  // object. This permits many Elm programs to be embedded per page.
  var elm = {
      notify:notify,
      node:container,
      display:display,
      id:ElmRuntime.guid(),
      addListener:addListener,
      inputs:inputs
  };

  // Set up methods to communicate with Elm program from JS.
  function send(name, value) {
      emitter.emit(name + '_' + elm.id, value)
  }
  function recv(name, handler) {
      emitter.on(name + '_' + elm.id, handler);
  }

  recv('log', function(e) {console.log(e.value)});

  function swap(newModule) {
      removeListeners(listeners);
      var div = document.createElement('div');
      var newElm = init(display, div, newModule, elm);
      inputs = [];
      // elm.send = newElm.send;
      // elm.recv = newElm.recv;
      // elm.swap = newElm.swap;
      return newElm;
  }

  var Module = {};
  var reportAnyErrors = function() {};
  try {
      Module = module(elm);
  } catch(e) {
      var directions = "<br/>&nbsp; &nbsp; Open the developer console for more details."
      Module.main = Elm.Text(elm).text('<code>' + e.message + directions + '</code>');
      reportAnyErrors = function() { throw e; }
  }
  inputs = ElmRuntime.filterDeadInputs(inputs);
  filterListeners(inputs, listeners);
  if (typeof moduleToReplace !== 'undefined') {
      ElmRuntime.swap(moduleToReplace, elm);
  }

  reportAnyErrors();
  return { send:send, recv:recv, swap:swap };
};

function filterListeners(inputs, listeners) {
    loop:
    for (var i = listeners.length; i--; ) {
        var listener = listeners[i];
        for (var j = inputs.length; j--; ) {
            if (listener.relevantInputs.indexOf(inputs[j].id) >= 0) {
                continue loop;
            }
        }
        listener.domNode.removeEventListener(listener.eventName, listener.func);
    }
}

function removeListeners(listeners) {
    for (var i = listeners.length; i--; ) {
        var listener = listeners[i];
        listener.domNode.removeEventListener(listener.eventName, listener.func);
    }
}

}());
