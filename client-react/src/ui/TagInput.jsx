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
var __spread = (this && this.__spread) || function () {
    for (var ar = [], i = 0; i < arguments.length; i++) ar = ar.concat(__read(arguments[i]));
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
var Input_1 = require("./Input");
var Icon_1 = require("./Icon");
var TagContent = styled_components_1.default.div(templateObject_1 || (templateObject_1 = __makeTemplateObject(["\n  display: inline-block;\n  color: ", ";\n  background-color: ", ";\n  padding: 4px;\n  border-radius: 2px 0 0 2px;\n"], ["\n  display: inline-block;\n  color: ", ";\n  background-color: ", ";\n  padding: 4px;\n  border-radius: 2px 0 0 2px;\n"])), color_1.colors.white, color_1.colors.blue200);
var TagsContainer = styled_components_1.default.div(templateObject_2 || (templateObject_2 = __makeTemplateObject(["\n  display: flex;\n  padding-bottom: 10px;\n"], ["\n  display: flex;\n  padding-bottom: 10px;\n"])));
var TagContainer = styled_components_1.default.div(templateObject_3 || (templateObject_3 = __makeTemplateObject(["\n  display: flex;\n"], ["\n  display: flex;\n"])));
var TagRemoveButton = styled_components_1.default.button(templateObject_4 || (templateObject_4 = __makeTemplateObject(["\n  background-color: ", ";\n  border: 0;\n  cursor: pointer;\n  margin-right: 7px;\n  height: 26px;\n  color: ", ";\n  border-radius: 0 2px 2px 0;\n  &:focus {\n    outline: none;\n  }\n"], ["\n  background-color: ", ";\n  border: 0;\n  cursor: pointer;\n  margin-right: 7px;\n  height: 26px;\n  color: ", ";\n  border-radius: 0 2px 2px 0;\n  &:focus {\n    outline: none;\n  }\n"])), color_1.colors.blue300, color_1.colors.white);
exports.TagInput = function (_a) {
    var tags = _a.tags, placeholder = _a.placeholder, onRemoveTag = _a.onRemoveTag, onAddTag = _a.onAddTag;
    var input = Input_1.useInput({
        trim: true,
        validations: [],
        shouldShowError: function (_) { return false; }
    });
    return (<div>
      <Input_1.Input {...input} placeholder={placeholder} onKeyDown={function (e) {
        if (e.key === 'Enter') {
            onAddTag(input.value);
            input.setValue('');
        }
    }}/>
      {tags.length > 0 && (<TagsContainer>
          {tags.map(function (tag) { return (<TagContainer>
              <TagContent>{tag}</TagContent>
              <TagRemoveButton onClick={function () { return onRemoveTag(tag); }}>
                <Icon_1.Icons.times />
              </TagRemoveButton>
            </TagContainer>); })}
        </TagsContainer>)}
    </div>);
};
exports.useTagInput = function () {
    var _a = __read(react_1.useState([]), 2), tags = _a[0], setTags = _a[1];
    return {
        tags: tags,
        onAddTag: function (tag) {
            setTags(__spread(new Set(__spread(tags, [tag]))));
        },
        onRemoveTag: function (tag) { return setTags(tags.filter(function (x) { return x !== tag; })); }
    };
};
var templateObject_1, templateObject_2, templateObject_3, templateObject_4;
