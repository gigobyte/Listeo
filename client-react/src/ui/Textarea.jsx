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
var Error_1 = require("./Error");
var validate_1 = require("./validate");
var TextareaWrapper = styled_components_1.default.textarea(templateObject_1 || (templateObject_1 = __makeTemplateObject(["\n  border-radius: 2px;\n  padding: 7px;\n  width: 200px;\n  padding-left: 15px;\n  font-size: 16px;\n  margin-bottom: 10px;\n  height: 25px;\n  min-height: 75px;\n  max-height: 150px;\n  border: 0;\n  transition: border-color 1000ms;\n  font-family: 'Museo-Sans';\n  background-color: ", ";\n  box-shadow: 1px 2px 3px 0 ", ";\n  &:focus {\n    outline: 0;\n    box-shadow: 1px 2px 3px 1px ", ";\n  }\n  ", ";\n"], ["\n  border-radius: 2px;\n  padding: 7px;\n  width: 200px;\n  padding-left: 15px;\n  font-size: 16px;\n  margin-bottom: 10px;\n  height: 25px;\n  min-height: 75px;\n  max-height: 150px;\n  border: 0;\n  transition: border-color 1000ms;\n  font-family: 'Museo-Sans';\n  background-color: ", ";\n  box-shadow: 1px 2px 3px 0 ", ";\n  &:focus {\n    outline: 0;\n    box-shadow: 1px 2px 3px 1px ", ";\n  }\n  ",
    ";\n"])), color_1.colors.gray100, color_1.colors.gray300, color_1.colors.gray300, function (props) {
    return props.error
        ? "\n    margin-bottom: 5px;\n    border-color: " + color_1.colors.crimson100 + ";\n    "
        : '';
});
exports.Textarea = function (props) { return (<>
    <TextareaWrapper {...props}/>
    <Error_1.Error visible={!!props.error && props.shouldShowError}>
      {props.error}
    </Error_1.Error>
  </>); };
exports.useTextarea = function (_a) {
    var validations = _a.validations, trim = _a.trim, shouldShowError = _a.shouldShowError;
    var _b = __read(react_1.useState(''), 2), value = _b[0], setValue = _b[1];
    var error = validate_1.validate(validations, value) || '';
    return {
        value: value,
        shouldShowError: shouldShowError(value),
        error: error,
        isValid: !error,
        setValue: function (newValue) { return setValue(trim ? newValue.trim() : newValue); },
        onChange: function (e) {
            return setValue(trim ? e.target.value.trim() : e.target.value);
        }
    };
};
var templateObject_1;
