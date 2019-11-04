"use strict";
var __makeTemplateObject = (this && this.__makeTemplateObject) || function (cooked, raw) {
    if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
    return cooked;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.fullHeight = function (el) {
    return el(templateObject_1 || (templateObject_1 = __makeTemplateObject(["\n    height: 100%\n    "], ["\n    height: 100%\n    "])));
};
exports.centered = function (el) {
    return el(templateObject_2 || (templateObject_2 = __makeTemplateObject(["\n    display: flex;\n    justify-content: center;\n    align-items: center;\n    flex-direction: column;\n    "], ["\n    display: flex;\n    justify-content: center;\n    align-items: center;\n    flex-direction: column;\n    "])));
};
var templateObject_1, templateObject_2;
