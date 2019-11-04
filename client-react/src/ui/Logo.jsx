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
var Link_1 = require("./Link");
var route_1 = require("../route");
var LogoWrapper = styled_components_1.default.span(templateObject_1 || (templateObject_1 = __makeTemplateObject(["\n  font-family: 'Candara Regular';\n  color: ", ";\n  font-weight: bold;\n  font-size: 3rem;\n"], ["\n  font-family: 'Candara Regular';\n  color: ", ";\n  font-weight: bold;\n  font-size: 3rem;\n"])), color_1.colors.blue200);
exports.Logo = function () { return (<Link_1.Link to={route_1.routes.home}>
    <LogoWrapper>Listeo</LogoWrapper>
  </Link_1.Link>); };
var templateObject_1;
