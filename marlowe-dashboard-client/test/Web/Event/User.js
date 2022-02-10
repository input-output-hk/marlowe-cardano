"use strict";

// So that we have a "document"
require("global-jsdom/register");
const userEvent = require("@testing-library/user-event").default;

const click = (user) => (e) => () => user.click(e).catch(console.error);
const dblClick = (user) => (e) => () => user.dblClick(e);
const tripleClick = (user) => (e) => () => user.tripleClick(e);
const hover = (user) => (e) => () => user.hover(e);
const unhover = (user) => (e) => () => user.unhover(e);
const tab = (user) => (ops) => () => user.tab(ops);
const keyboard = (user) => (text) => () => user.keyboard(text);
const copy = (user) => user.copy;
const cut = (user) => user.cut;
const paste = (user) => (data) => () => user.paste(data);
const pointer = (user) => (inp) => () => user.pointer(inp);
const clear = (user) => (e) => () => user.clear(e);
const deselectOptions = (user) => (e) => (ops) => () =>
  user.deselectOptions(e, ops);
const selectOptions = (user) => (e) => (ops) => () =>
  user.selectOptions(e, ops);
const type = (user) => (e) => (text) => (ops) => () => user.type(e, text, ops);
const upload = (user) => (e) => (files) => () => user.type(e, files);

function adaptUser(user, options) {
  return {
    click: click(user),
    dblClick: dblClick(user),
    tripleClick: tripleClick(user),
    hover: hover(user),
    unhover: unhover(user),
    tab: tab(user),
    keyboard: keyboard(user),
    copy: copy(user),
    cut: cut(user),
    cut: cut(user),
    paste: paste(user),
    pointer: pointer(user),
    clear: clear(user),
    deselectOptions: deselectOptions(user),
    selectOptions: selectOptions(user),
    type: type(user),
    upload: upload(user),
    setup: function (options) {
      return function () {
        return adaptUser(user.setup(options));
      };
    },
  };
}

exports._setup = function (options) {
  return function () {
    const user = userEvent.setup(options);
    return adaptUser(user);
  };
};
