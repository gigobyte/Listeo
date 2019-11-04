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
var react_use_1 = require("react-use");
var styled_components_1 = require("styled-components");
var redux_starter_kit_1 = require("redux-starter-kit");
var react_redux_1 = require("react-redux");
var session_1 = require("./session");
var route_1 = require("./route");
var color_1 = require("./ui/color");
var http_1 = require("./http");
var Header_1 = require("./ui/Header");
var MuseoSans_100_ttf_1 = __importDefault(require("./assets/MuseoSans-100.ttf"));
var Login_1 = require("./pages/Login");
var Register_1 = require("./pages/Register");
var CreatePlaylist_1 = require("./pages/playlist/CreatePlaylist");
var store = redux_starter_kit_1.configureStore({
    reducer: session_1.session.reducer
});
exports.App = function () {
    return (<react_redux_1.Provider store={store}>
      <exports.Main />
    </react_redux_1.Provider>);
};
var GlobalStyle = styled_components_1.createGlobalStyle(templateObject_1 || (templateObject_1 = __makeTemplateObject(["\n  @font-face {\n    font-family: Museo-Sans;\n    src: url(", ");\n  }\n\n  html, body {\n    height: 100%;\n    margin: 0;\n    font-family: 'Museo-Sans';\n    background-color: ", ";\n  }\n\n  main {\n    height: 100%;\n    display: flex;\n    flex-direction: column;\n  }\n"], ["\n  @font-face {\n    font-family: Museo-Sans;\n    src: url(", ");\n  }\n\n  html, body {\n    height: 100%;\n    margin: 0;\n    font-family: 'Museo-Sans';\n    background-color: ", ";\n  }\n\n  main {\n    height: 100%;\n    display: flex;\n    flex-direction: column;\n  }\n"])), MuseoSans_100_ttf_1.default, color_1.colors.gray200);
var Layout = function (_a) {
    var children = _a.children;
    return (<>
    <GlobalStyle />
    <Header_1.Header />
    {children}
  </>);
};
exports.Main = function () {
    var dispatch = react_redux_1.useDispatch();
    react_use_1.useMount(function () {
        dispatch(session_1.session.effects.fetchUser());
    });
    var user = session_1.useUser();
    var route = session_1.useRoute();
    switch (user.status) {
        case http_1.DataStatus.NotAsked:
        case http_1.DataStatus.Loading:
            return null;
        default: {
            switch (route) {
                case route_1.routes.home:
                    return <Layout></Layout>;
                case route_1.routes.login:
                    return (<Layout>
              <Login_1.Login />
            </Layout>);
                case route_1.routes.register:
                    return (<Layout>
              <Register_1.Register />
            </Layout>);
                case route_1.routes.createPlaylist:
                    return (<Layout>
              <CreatePlaylist_1.CreatePlaylist />
            </Layout>);
                case route_1.routes.notFound404:
                default:
                    return null;
            }
        }
    }
};
var templateObject_1;
