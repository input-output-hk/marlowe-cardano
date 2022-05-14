/*eslint-env node*/
"use strict";
const { registerDateTimeField } = require("src/Blockly/DateTimeField.js");

exports.createBlocklyInstance_ = function () {
  return require("blockly/blockly-node");
};

exports.createWorkspace_ = function (blockly) {
  return new blockly.Workspace();
};

exports.initializeWorkspace_ = function (blockly, workspace) {
  try {
    blockly.Extensions.register("timeout_validator", function () {});
  } catch (err) {}
  try {
    blockly.Extensions.register("hash_validator", function () {});
  } catch (err) {}
  try {
    blockly.Extensions.register("number_validator", function () {});
  } catch (err) {}
  // FIXME
  try {
    registerDateTimeField(blockly);
  } catch (err) {}
  // const FieldDateTime = function () {}
  // FieldDateTime.
  // blockly.fieldRegistry.register('field_datetime', FieldDateTime);
  try {
    blockly.Extensions.register("dynamic_timeout_type", function () {});
  } catch (err) {}

  var xmlText =
    '<xml id="workspaceBlocks" style="display:none"><block type="BaseContractType" x="13" y="187" id="root_contract"></block></xml>';
  var workspaceBlocks = blockly.Xml.textToDom(xmlText);
  blockly.Xml.domToWorkspace(workspaceBlocks, workspace);
  workspace.getAllBlocks()[0].setDeletable(false);
};

exports.newBlock_ = function (workspace, name) {
  return workspace.newBlock(name);
};
