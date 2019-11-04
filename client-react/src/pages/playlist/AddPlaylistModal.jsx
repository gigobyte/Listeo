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
var Container_1 = require("../../ui/Container");
var color_1 = require("../../ui/color");
var Icon_1 = require("../../ui/Icon");
var Button_1 = require("../../ui/Button");
var react_redux_1 = require("react-redux");
var session_1 = require("../../session");
var route_1 = require("../../route");
var Modal_1 = require("../../ui/Modal");
var Container = styled_components_1.default(Container_1.fullHeight(styled_components_1.default.div))(templateObject_1 || (templateObject_1 = __makeTemplateObject(["\n  display: flex;\n  flex-direction: column;\n"], ["\n  display: flex;\n  flex-direction: column;\n"])));
var OptionsContainer = styled_components_1.default.div(templateObject_2 || (templateObject_2 = __makeTemplateObject(["\n  display: flex;\n  padding-top: 5%;\n  height: 100%;\n  justify-content: center;\n"], ["\n  display: flex;\n  padding-top: 5%;\n  height: 100%;\n  justify-content: center;\n"])));
var Header = styled_components_1.default.div(templateObject_3 || (templateObject_3 = __makeTemplateObject(["\n  text-align: center;\n"], ["\n  text-align: center;\n"])));
var Title = styled_components_1.default.span(templateObject_4 || (templateObject_4 = __makeTemplateObject(["\n  text-transform: uppercase;\n"], ["\n  text-transform: uppercase;\n"])));
var Subtitle = styled_components_1.default.span(templateObject_5 || (templateObject_5 = __makeTemplateObject(["\n  font-size: 1.4rem;\n"], ["\n  font-size: 1.4rem;\n"])));
var OptionCard = styled_components_1.default.div(templateObject_6 || (templateObject_6 = __makeTemplateObject(["\n  flex-basis: 25%;\n  display: flex;\n  flex-direction: column;\n  align-items: center;\n  padding: 0 10px;\n  &:hover i {\n    transform: scale(1.1);\n  }\n"], ["\n  flex-basis: 25%;\n  display: flex;\n  flex-direction: column;\n  align-items: center;\n  padding: 0 10px;\n  &:hover i {\n    transform: scale(1.1);\n  }\n"])));
var optionIconStyle = "\n  text-align: center;\n  font-size: 5rem;\n  color: " + color_1.colors.blue200 + ";\n  transition: transform 300ms;\n";
var NewPlaylistIcon = styled_components_1.default(Icon_1.Icons.folderPlus)(templateObject_7 || (templateObject_7 = __makeTemplateObject(["\n  ", "\n"], ["\n  ", "\n"])), optionIconStyle);
var ImportPlaylistIcon = styled_components_1.default(Icon_1.Icons.cloudDownload)(templateObject_8 || (templateObject_8 = __makeTemplateObject(["\n  ", "\n"], ["\n  ", "\n"])), optionIconStyle);
var OptionDescription = styled_components_1.default.div(templateObject_9 || (templateObject_9 = __makeTemplateObject(["\n  margin-top: 20px;\n  margin-bottom: 25px;\n  text-align: center;\n"], ["\n  margin-top: 20px;\n  margin-bottom: 25px;\n  text-align: center;\n"])));
exports.AddPlaylistModal = function (_a) {
    var onClose = _a.onClose;
    var dispatch = react_redux_1.useDispatch();
    return (<Modal_1.Modal onClose={onClose}>
      <Container>
        <Header>
          <div>
            <Title>New playlist</Title>
          </div>
          <div>
            <Subtitle>Choose a starting point</Subtitle>
          </div>
        </Header>
        <OptionsContainer>
          <OptionCard>
            <NewPlaylistIcon />
            <OptionDescription>Start with an empty playlist.</OptionDescription>
            <Button_1.Button onClick={function () {
        onClose();
        dispatch(session_1.session.effects.redirect(route_1.routes.createPlaylist));
    }}>
              Create new
            </Button_1.Button>
          </OptionCard>
          <OptionCard>
            <ImportPlaylistIcon />
            <OptionDescription>
              Import your existing playlist.
            </OptionDescription>
            <Button_1.Button disabled>Import</Button_1.Button>
          </OptionCard>
        </OptionsContainer>
      </Container>
    </Modal_1.Modal>);
};
var templateObject_1, templateObject_2, templateObject_3, templateObject_4, templateObject_5, templateObject_6, templateObject_7, templateObject_8, templateObject_9;
