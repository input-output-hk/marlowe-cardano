/*eslint-env node*/
/*global global*/
// FIXME: We should check where we use this and replace it with google font icons
import "@fortawesome/fontawesome-free/css/all.css";
import "./static/css/main.css";
import "blockly";
import "./grammar.ne";
import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import { EmacsExtension } from "monaco-emacs";
import { initVimMode } from "monaco-vim";
import * as bignumberDTS from "!!raw-loader!bignumber.js/bignumber.d.ts";
import * as marloweDTS from "!!raw-loader!src/Language/Javascript/MarloweJS.ts";
import { BigNumber } from "bignumber";
import { stringify, parse } from "json-bigint";
import { main } from "./output/Main";

global.monaco = monaco;
global.EmacsExtension = EmacsExtension;
global.initVimMode = initVimMode;
global.monacoExtraTypeScriptLibs = [
  [bignumberDTS.default, "inmemory://model/bignumber.js.d.ts"],
  [marloweDTS.default, "inmemory://model/marlowe-js.d.ts"],
];

JSON.stringify = stringify;
JSON.parse = parse;

main();
