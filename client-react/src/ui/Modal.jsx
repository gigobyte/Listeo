"use strict";
var __makeTemplateObject = (this && this.__makeTemplateObject) || function (cooked, raw) {
    if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
    return cooked;
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var react_1 = __importDefault(require("react"));
var styled_components_1 = __importDefault(require("styled-components"));
var color_1 = require("./color");
var Icon_1 = require("./Icon");
var Overlay = styled_components_1.default.div(templateObject_1 || (templateObject_1 = __makeTemplateObject(["\n  position: fixed;\n  top: 0;\n  right: 0;\n  bottom: 0;\n  left: 0;\n  z-index: 999;\n  overflow: hidden;\n"], ["\n  position: fixed;\n  top: 0;\n  right: 0;\n  bottom: 0;\n  left: 0;\n  z-index: 999;\n  overflow: hidden;\n"])));
var Container = styled_components_1.default.div(templateObject_2 || (templateObject_2 = __makeTemplateObject(["\n  @keyframes fadeIn {\n    0% {\n      opacity: 0;\n    }\n\n    100% {\n      opacity: 1;\n    }\n  }\n\n  position: relative;\n  width: 50%;\n  margin: auto;\n  top: 20%;\n  background-color: ", ";\n  padding: 40px 0;\n  border-radius: 5px;\n  animation-name: fadeIn;\n  animation-duration: 200ms;\n"], ["\n  @keyframes fadeIn {\n    0% {\n      opacity: 0;\n    }\n\n    100% {\n      opacity: 1;\n    }\n  }\n\n  position: relative;\n  width: 50%;\n  margin: auto;\n  top: 20%;\n  background-color: ", ";\n  padding: 40px 0;\n  border-radius: 5px;\n  animation-name: fadeIn;\n  animation-duration: 200ms;\n"])), color_1.colors.white);
var Backdrop = styled_components_1.default.div(templateObject_3 || (templateObject_3 = __makeTemplateObject(["\n  position: fixed;\n  top: 0;\n  right: 0;\n  bottom: 0;\n  left: 0;\n  opacity: 0.3;\n  background-color: ", ";\n"], ["\n  position: fixed;\n  top: 0;\n  right: 0;\n  bottom: 0;\n  left: 0;\n  opacity: 0.3;\n  background-color: ", ";\n"])), color_1.colors.black);
var CloseIcon = styled_components_1.default(Icon_1.Icons.times)(templateObject_4 || (templateObject_4 = __makeTemplateObject(["\n  position: absolute;\n  top: 10px;\n  right: 10px;\n  text-align: right;\n  color: ", ";\n  cursor: pointer;\n"], ["\n  position: absolute;\n  top: 10px;\n  right: 10px;\n  text-align: right;\n  color: ", ";\n  cursor: pointer;\n"])), color_1.colors.blue200);
exports.Modal = function (_a) {
    var onClose = _a.onClose, children = _a.children;
    return (<>
    <Overlay>
      <Container>
        <CloseIcon onClick={onClose}/>
        {children}
      </Container>
    </Overlay>
    <Backdrop />
  </>);
};
var templateObject_1, templateObject_2, templateObject_3, templateObject_4;
