"use strict";
var __makeTemplateObject = (this && this.__makeTemplateObject) || function (cooked, raw) {
    if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
    return cooked;
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var styled_components_1 = __importDefault(require("styled-components"));
var color_1 = require("./color");
exports.Button = styled_components_1.default.button(templateObject_1 || (templateObject_1 = __makeTemplateObject(["\n  border-radius: 44px;\n  border: 0;\n  background-image: linear-gradient(\n    310deg,\n    ", ",\n    ", "\n  );\n  transition: transform 300ms;\n  color: ", ";\n  padding: 10px 20px;\n  font-weight: bold;\n  font-size: 1rem;\n  font-family: 'Museo-Sans';\n  cursor: pointer;\n  &:focus {\n    outline: none;\n  }\n  &:active {\n    transform: scale(0.95);\n  }\n  &:disabled {\n    cursor: not-allowed;\n    opacity: 0.7;\n  }\n"], ["\n  border-radius: 44px;\n  border: 0;\n  background-image: linear-gradient(\n    310deg,\n    ", ",\n    ", "\n  );\n  transition: transform 300ms;\n  color: ", ";\n  padding: 10px 20px;\n  font-weight: bold;\n  font-size: 1rem;\n  font-family: 'Museo-Sans';\n  cursor: pointer;\n  &:focus {\n    outline: none;\n  }\n  &:active {\n    transform: scale(0.95);\n  }\n  &:disabled {\n    cursor: not-allowed;\n    opacity: 0.7;\n  }\n"])), color_1.colors.blue400, color_1.colors.blue300, color_1.colors.white);
var templateObject_1;
