"use strict";
var __makeTemplateObject = (this && this.__makeTemplateObject) || function (cooked, raw) {
    if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
    return cooked;
};
var __read = (this && this.__read) || function (o, n) {
    var m = typeof Symbol === "function" && o[Symbol.iterator];
    if (!m) return o;
    var i = m.call(o), r, ar = [], e;
    try {
        while ((n === void 0 || n-- > 0) && !(r = i.next()).done) ar.push(r.value);
    }
    catch (error) { e = { error: error }; }
    finally {
        try {
            if (r && !r.done && (m = i["return"])) m.call(i);
        }
        finally { if (e) throw e.error; }
    }
    return ar;
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
var styled_components_1 = __importDefault(require("styled-components"));
var color_1 = require("./color");
var Input = styled_components_1.default.input(templateObject_1 || (templateObject_1 = __makeTemplateObject(["\n  display: none;\n"], ["\n  display: none;\n"])));
var Label = styled_components_1.default.label(templateObject_2 || (templateObject_2 = __makeTemplateObject(["\n  position: relative;\n  display: inline-block;\n  padding-left: 25px;\n  height: 22px;\n\n  input:checked + span:after {\n    content: '';\n    height: 10px;\n    width: 10px;\n    background-color: ", ";\n    position: absolute;\n    border-radius: 50%;\n    left: 50%;\n    top: 50%;\n    transform: translate(-50%, -50%);\n  }\n"], ["\n  position: relative;\n  display: inline-block;\n  padding-left: 25px;\n  height: 22px;\n\n  input:checked + span:after {\n    content: '';\n    height: 10px;\n    width: 10px;\n    background-color: ", ";\n    position: absolute;\n    border-radius: 50%;\n    left: 50%;\n    top: 50%;\n    transform: translate(-50%, -50%);\n  }\n"])), color_1.colors.blue100);
var Circle = styled_components_1.default.span(templateObject_3 || (templateObject_3 = __makeTemplateObject(["\n  display: inline-block;\n  width: 18px;\n  height: 18px;\n  position: absolute;\n  left: 0;\n  top: -3px;\n  border-radius: 50%;\n  border: 2px solid ", ";\n"], ["\n  display: inline-block;\n  width: 18px;\n  height: 18px;\n  position: absolute;\n  left: 0;\n  top: -3px;\n  border-radius: 50%;\n  border: 2px solid ", ";\n"])), color_1.colors.blue100);
exports.RadioButton = function (_a) {
    var label = _a.label, checked = _a.checked, onClick = _a.onClick;
    return (<Label>
    {label}
    <Input type="radio" checked={checked} onClick={onClick}/>
    <Circle />
  </Label>);
};
exports.useRadioButtons = function (_a) {
    var initialValue = _a.initialValue, values = _a.values;
    var _b = __read(react_1.useState(initialValue), 2), value = _b[0], setValue = _b[1];
    var _c = __read(react_1.useState({}), 2), checkedValues = _c[0], setCheckedValue = _c[1];
    return {
        value: value,
        radioButtons: values.map(function (value, i) { return ({
            checked: checkedValues[i],
            onClick: function () {
                var _a;
                setCheckedValue((_a = {}, _a[i] = true, _a));
                setValue(value);
            }
        }); })
    };
};
var templateObject_1, templateObject_2, templateObject_3;
