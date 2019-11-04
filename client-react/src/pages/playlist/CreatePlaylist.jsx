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
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var react_1 = __importDefault(require("react"));
var styled_components_1 = __importDefault(require("styled-components"));
var Container_1 = require("../../ui/Container");
var react_use_1 = require("react-use");
var Input_1 = require("../../ui/Input");
var validate_1 = require("../../ui/validate");
var useForm_1 = require("../../ui/useForm");
var TagInput_1 = require("../../ui/TagInput");
var Textarea_1 = require("../../ui/Textarea");
var RadioButton_1 = require("../../ui/RadioButton");
var Button_1 = require("../../ui/Button");
var http_1 = require("../../http");
var endpoint_1 = require("../../endpoint");
var session_1 = require("../../session");
var react_redux_1 = require("react-redux");
var route_1 = require("../../route");
var ValidationError;
(function (ValidationError) {
    ValidationError["PlaylistNameMissing"] = "Please enter the name of the playlist";
})(ValidationError || (ValidationError = {}));
var PlaylistPrivacy;
(function (PlaylistPrivacy) {
    PlaylistPrivacy[PlaylistPrivacy["Public"] = 0] = "Public";
    PlaylistPrivacy[PlaylistPrivacy["Private"] = 1] = "Private";
})(PlaylistPrivacy || (PlaylistPrivacy = {}));
var PlaylistStyle;
(function (PlaylistStyle) {
    PlaylistStyle[PlaylistStyle["Ranked"] = 0] = "Ranked";
    PlaylistStyle[PlaylistStyle["Unordered"] = 1] = "Unordered";
})(PlaylistStyle || (PlaylistStyle = {}));
var CreatePlaylistResponseError;
(function (CreatePlaylistResponseError) {
    CreatePlaylistResponseError["InvalidRequest"] = "InvalidRequest";
    CreatePlaylistResponseError["ValidationFailed"] = "ValidationFailed";
})(CreatePlaylistResponseError || (CreatePlaylistResponseError = {}));
var createPlaylistEndpoint = endpoint_1.createEndpoint('/playlist');
var Container = styled_components_1.default(Container_1.centered(styled_components_1.default.div))(templateObject_1 || (templateObject_1 = __makeTemplateObject(["\n  height: 66%;\n"], ["\n  height: 66%;\n"])));
var Title = styled_components_1.default.h1(templateObject_2 || (templateObject_2 = __makeTemplateObject(["\n  font-size: 2rem;\n"], ["\n  font-size: 2rem;\n"])));
var SettingsColumn = styled_components_1.default.div(templateObject_3 || (templateObject_3 = __makeTemplateObject(["\n  display: flex;\n  flex-direction: column;\n  padding-right: 15px;\n  &:last-child {\n    padding-right: 0;\n  }\n"], ["\n  display: flex;\n  flex-direction: column;\n  padding-right: 15px;\n  &:last-child {\n    padding-right: 0;\n  }\n"])));
var SettingLabel = styled_components_1.default.span(templateObject_4 || (templateObject_4 = __makeTemplateObject(["\n  font-weight: bold;\n"], ["\n  font-weight: bold;\n"])));
var Settings = styled_components_1.default.div(templateObject_5 || (templateObject_5 = __makeTemplateObject(["\n  display: flex;\n  padding: 10px 0;\n"], ["\n  display: flex;\n  padding: 10px 0;\n"])));
var Separator = styled_components_1.default.div(templateObject_6 || (templateObject_6 = __makeTemplateObject(["\n  height: 5px;\n"], ["\n  height: 5px;\n"])));
exports.CreatePlaylist = function () {
    react_use_1.useTitle('Create Playlist - Listeo');
    var http = http_1.useHttp();
    var dispatch = react_redux_1.useDispatch();
    var createPlaylistForm = useForm_1.useForm({
        onSubmit: function () {
            if (playlistNameInput.isValid) {
                http
                    .post(createPlaylistEndpoint, {
                    name: playlistNameInput.value,
                    description: descriptionInput.value,
                    tags: tagsInput.tags,
                    privacy: playlistPrivacy.value,
                    style: playlistStyle.value
                })
                    .then(function (response) {
                    dispatch(session_1.session.effects.redirect(route_1.routes.viewPlaylist(response.playlistId)));
                });
            }
        }
    });
    var playlistNameInput = Input_1.useInput({
        trim: false,
        validations: [validate_1.rule(validate_1.ifBlank, ValidationError.PlaylistNameMissing)],
        shouldShowError: function (_) { return createPlaylistForm.submitted; }
    });
    var tagsInput = TagInput_1.useTagInput();
    var descriptionInput = Textarea_1.useTextarea({
        trim: false,
        validations: [],
        shouldShowError: function (_) { return false; }
    });
    var playlistPrivacy = RadioButton_1.useRadioButtons({
        initialValue: PlaylistPrivacy.Public,
        values: [PlaylistPrivacy.Public, PlaylistPrivacy.Private]
    });
    var _a = __read(playlistPrivacy.radioButtons, 2), publicRadioButton = _a[0], privateRadioButton = _a[1];
    var playlistStyle = RadioButton_1.useRadioButtons({
        initialValue: PlaylistStyle.Unordered,
        values: [PlaylistStyle.Ranked, PlaylistStyle.Unordered]
    });
    var _b = __read(playlistStyle.radioButtons, 2), rankedRadioButton = _b[0], unorderedRadioButton = _b[1];
    return (<Container>
      <Title>Create a new playlist</Title>
      <Input_1.Input {...playlistNameInput} placeholder="Name of list"/>
      <TagInput_1.TagInput {...tagsInput} placeholder="Tags (optional)"/>
      <Textarea_1.Textarea {...descriptionInput} placeholder="Description (optional)"/>
      <Settings>
        <SettingsColumn>
          <SettingLabel>Privacy</SettingLabel>
          <Separator />
          <SettingLabel>Style</SettingLabel>
        </SettingsColumn>
        <SettingsColumn>
          <RadioButton_1.RadioButton {...publicRadioButton} label="Public"/>
          <RadioButton_1.RadioButton {...rankedRadioButton} label="Ranked"/>
        </SettingsColumn>
        <SettingsColumn>
          <RadioButton_1.RadioButton {...privateRadioButton} label="Private"/>
          <RadioButton_1.RadioButton {...unorderedRadioButton} label="Unordered"/>
        </SettingsColumn>
      </Settings>
      <Button_1.Button onClick={createPlaylistForm.onSubmit}>Create</Button_1.Button>
    </Container>);
};
var templateObject_1, templateObject_2, templateObject_3, templateObject_4, templateObject_5, templateObject_6;
