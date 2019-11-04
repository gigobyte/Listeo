"use strict";
var __makeTemplateObject = (this && this.__makeTemplateObject) || function (cooked, raw) {
    if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
    return cooked;
};
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (Object.hasOwnProperty.call(mod, k)) result[k] = mod[k];
    result["default"] = mod;
    return result;
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var react_1 = __importStar(require("react"));
var route_1 = require("../route");
var styled_components_1 = __importDefault(require("styled-components"));
var color_1 = require("./color");
var react_redux_1 = require("react-redux");
var session_1 = require("../session");
var LinkWrapper = styled_components_1.default.a(templateObject_1 || (templateObject_1 = __makeTemplateObject(["\n  text-decoration: none;\n  color: ", ";\n"], ["\n  text-decoration: none;\n  color: ", ";\n"])), color_1.colors.blue200);
exports.Link = function (_a) {
    var to = _a.to, children = _a.children;
    var dispatch = react_redux_1.useDispatch();
    var handleClick = react_1.useCallback(function (e) {
        e.preventDefault();
        dispatch(session_1.session.effects.redirect(to));
    }, [to]);
    return (<LinkWrapper onClick={handleClick} href={route_1.routeToString(to)}>
      {children}
    </LinkWrapper>);
};
var templateObject_1;
