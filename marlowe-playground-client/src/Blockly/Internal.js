/*eslint-env node*/
"use strict";

var JSONbig = require("json-bigint");

exports.createBlocklyInstance_ = () => {
  return require("blockly");
};

exports.debugBlockly = (name) => (state) => () => {
  if (typeof window.blockly === "undefined") {
    window.blockly = {};
  }
  window.blockly[name] = state;
};

exports.createWorkspace = (blockly) => (workspaceDiv) => (config) => () => {
  /* Disable comments */
  try {
    blockly.ContextMenuRegistry.registry.unregister("blockComment");
  } catch (err) {}

  /* Disable disabling blocks */
  try {
    blockly.ContextMenuRegistry.registry.unregister("blockDisable");
  } catch (err) {}

  /* Register extensions */
  /* Silently clean if already registered */
  try {
    blockly.Extensions.register("timeout_validator", function () {});
  } catch (err) {}
  blockly.Extensions.unregister("timeout_validator");
  try {
    blockly.Extensions.register("hash_validator", function () {});
  } catch (err) {}
  blockly.Extensions.unregister("hash_validator");
  try {
    blockly.Extensions.register("number_validator", function () {});
  } catch (err) {}
  blockly.Extensions.unregister("number_validator");

  /* Timeout extension (advanced validation for the timeout field) */
  blockly.Extensions.register("timeout_validator", function () {
    var thisBlock = this;

    /* Validator for timeout */
    var timeoutValidator = function (input) {
      if (thisBlock.getFieldValue("timeout_type") == "slot") {
        var cleanedInput = input.replace(new RegExp("[,]+", "g"), "").trim();
        if (new RegExp("^(-[0-9])?[0-9]*$", "g").test(cleanedInput)) {
          return BigInt(cleanedInput).toString();
        } else {
          return null;
        }
      } else {
        return input;
      }
    };

    thisBlock.getField("timeout").setValidator(timeoutValidator);

    /* This sets the timeout to zero when switching to slot in the dropdown */
    this.setOnChange(function (event) {
      if (
        event.blockId == thisBlock.id &&
        event.name == "timeout_type" &&
        event.element == "field" &&
        event.oldValue != event.newValue
      ) {
        if (timeoutValidator(thisBlock.getFieldValue("timeout")) === null) {
          thisBlock.setFieldValue("0", "timeout");
        }
      }
    });
  });

  /* Hash extension (advanced validation for the hash fields) */
  blockly.Extensions.register("hash_validator", function () {
    var thisBlock = this;

    /* Validator for hash */
    var hashValidator = function (input) {
      var cleanedInput = input
        .replace(new RegExp("[^a-fA-F0-9]+", "g"), "")
        .toLowerCase();
      if (new RegExp("^([a-f0-9][a-f0-9])*$", "g").test(cleanedInput)) {
        return cleanedInput;
      } else {
        return null;
      }
    };

    ["currency_symbol", "pubkey"].forEach(function (fieldName) {
      var field = thisBlock.getField(fieldName);
      if (field != null) {
        field.setValidator(hashValidator);
      }
    });
  });

  /* Number extension (advanced validation for number fields - other than timeout) */
  blockly.Extensions.register("number_validator", function () {
    var thisBlock = this;

    /* Validator for number fields */
    var numberValidator = function (input) {
      if (!isFinite(input)) {
        return null;
      }
    };

    thisBlock.inputList.forEach((input) => {
      input.fieldRow.forEach((field) => {
        if (field instanceof blockly.FieldNumber) {
          field.setValidator(numberValidator);
        }
      });
    });
  });

  /* Inject workspace */
  var workspace = blockly.inject(workspaceDiv, config);
  blockly.svgResize(workspace);

  return workspace;
};

exports.resize = (blockly) => (workspace) => () => {
  blockly.svgResize(workspace);
  workspace.render();
};

function removeUndefinedFields(obj) {
  for (var propName in obj) {
    if (obj[propName] === undefined) {
      delete obj[propName];
    }
  }
}

function removeEmptyArrayFields(obj) {
  for (var propName in obj) {
    if (Array.isArray(obj[propName]) && obj[propName].length == 0) {
      delete obj[propName];
    }
  }
}

exports.addBlockType_ = (blockly) => (name) => (block) => () => {
  // we really don't want to be mutating the input object, it is not supposed to be state
  var clone = JSONbig.parse(JSONbig.stringify(block));
  removeUndefinedFields(clone);
  removeEmptyArrayFields(clone);
  blockly.Blocks[name] = {
    init: function () {
      this.jsonInit(clone);
    },
  };
};

exports.initializeWorkspace_ =
  (blockly) => (workspace) => (workspaceBlocks) => () => {
    blockly.Xml.domToWorkspace(workspaceBlocks, workspace);
    workspace.getAllBlocks()[0].setDeletable(false);
  };

exports.render = (workspace) => () => {
  workspace.render();
};

exports.getBlockById_ = (just) => (nothing) => (workspace) => (id) => () => {
  var result = workspace.getBlockById(id);
  if (result) {
    return just(result);
  } else {
    return nothing;
  }
};

exports.workspaceXML = (blockly) => (workspace) => () => {
  const isEmpty = workspace.getAllBlocks()[0].getChildren().length == 0;
  if (isEmpty) {
    return "";
  } else {
    var dom = blockly.Xml.workspaceToDom(workspace);
    return blockly.utils.xml.domToText(dom);
  }
};

exports.loadWorkspace = (blockly) => (workspace) => (xml) => () => {
  var dom = blockly.utils.xml.textToDomDocument(xml);
  blockly.Xml.clearWorkspaceAndLoadFromXml(dom.childNodes[0], workspace);
  workspace.getAllBlocks()[0].setDeletable(false);
};

exports.addChangeListener = (workspace) => (listener) => () => {
  workspace.addChangeListener(listener);
};

exports.removeChangeListener = (workspace) => (listener) => () => {
  workspace.removeChangeListener(listener);
};

exports.workspaceToDom = (blockly) => (workspace) => () => {
  return blockly.Xml.workspaceToDom(workspace);
};

exports.select = (block) => () => {
  block.select();
};

exports.centerOnBlock = (workspace) => (blockId) => () => {
  workspace.centerOnBlock(blockId);
};

exports.hideChaff = (blockly) => () => {
  blockly.hideChaff();
};

exports.getBlockType = (block) => {
  return block.type;
};

exports.updateToolbox_ = (toolboxJson) => (workspace) => () => {
  workspace.updateToolbox(toolboxJson);
};

exports.clearUndoStack = (workspace) => () => {
  workspace.clearUndo();
};

exports.isWorkspaceEmpty = (workspace) => () => {
  var topBlocks = workspace.getTopBlocks(false);
  return topBlocks == null || topBlocks.length == 0;
};

exports.setGroup = (blockly) => (isGroup) => () =>
  blockly.Events.setGroup(isGroup);

exports.inputList = (block) => {
  return block.inputList;
};

exports.connectToPrevious = (block) => (input) => () => {
  block.previousConnection.connect(input.connection);
};
exports.previousConnection = (block) => {
  return block.previousConnection;
};

exports.nextConnection = (block) => {
  return block.nextConnection;
};

exports.connect = (from) => (to) => () => {
  from.connect(to);
};

exports.connectToOutput = (block) => (input) => () => {
  block.outputConnection.connect(input.connection);
};

exports.newBlock = (workspace) => (name) => () => {
  var block = workspace.newBlock(name);
  block.initSvg();
  return block;
};

exports.inputName = (input) => {
  return input.name;
};

exports.inputType = (input) => {
  return input.type;
};

exports.clearWorkspace = (workspace) => () => {
  workspace.clear();
};

exports.fieldRow = (input) => {
  return input.fieldRow;
};

exports.setFieldText = (field) => (text) => () => {
  field.setValue(text);
};

exports.fieldName = (field) => {
  return field.name;
};
