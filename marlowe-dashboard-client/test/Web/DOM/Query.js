"use strict";

// So that we have a "document"
require("jsdom-global")(undefined, {
  url: "https://purescript.org",
  pretendToBeVisual: true,
});
const { query } = require("@testing-library/dom");

exports.findAllByAltText = query.findAllByAltText;
exports.findAllByDisplayValue = query.findAllByDisplayValue;
exports.findAllByLabelText = query.findAllByLabelText;
exports.findAllByPlaceholderText = query.findAllByPlaceholderText;
exports.findAllByRole = query.findAllByRole;
exports.findAllByTestId = query.findAllByTestId;
exports.findAllByText = query.findAllByText;
exports.findAllByTitle = query.findAllByTitle;
exports.findByAltText = query.findByAltText;
exports.findByDisplayValue = query.findByDisplayValue;
exports.findByLabelText = query.findByLabelText;
exports.findByPlaceholderText = query.findByPlaceholderText;
exports.findByRole = query.findByRole;
exports.findByTestId = query.findByTestId;
exports.findByText = query.findByText;
exports.findByTitle = query.findByTitle;
exports.getAllByAltText = query.getAllByAltText;
exports.getAllByDisplayValue = query.getAllByDisplayValue;
exports.getAllByLabelText = query.getAllByLabelText;
exports.getAllByPlaceholderText = query.getAllByPlaceholderText;
exports.getAllByRole = query.getAllByRole;
exports.getAllByTestId = query.getAllByTestId;
exports.getAllByText = query.getAllByText;
exports.getAllByTitle = query.getAllByTitle;
exports.getByAltText = query.getByAltText;
exports.getByDisplayValue = query.getByDisplayValue;
exports.getByLabelText = query.getByLabelText;
exports.getByPlaceholderText = query.getByPlaceholderText;
exports.getByRole = query.getByRole;
exports.getByTestId = query.getByTestId;
exports.getByText = query.getByText;
exports.getByTitle = query.getByTitle;
exports.queryAllByAltText = query.queryAllByAltText;
exports.queryAllByDisplayValue = query.queryAllByDisplayValue;
exports.queryAllByLabelText = query.queryAllByLabelText;
exports.queryAllByPlaceholderText = query.queryAllByPlaceholderText;
exports.queryAllByRole = query.queryAllByRole;
exports.queryAllByTestId = query.queryAllByTestId;
exports.queryAllByText = query.queryAllByText;
exports.queryAllByTitle = query.queryAllByTitle;
exports.queryByAltText = query.queryByAltText;
exports.queryByDisplayValue = query.queryByDisplayValue;
exports.queryByLabelText = query.queryByLabelText;
exports.queryByPlaceholderText = query.queryByPlaceholderText;
exports.queryByRole = query.queryByRole;
exports.queryByTestId = query.queryByTestId;
exports.queryByText = query.queryByText;
exports.queryByTitle = query.queryByTitle;
