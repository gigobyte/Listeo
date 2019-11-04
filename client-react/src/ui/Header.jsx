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
var session_1 = require("../session");
var http_1 = require("../http");
var Link_1 = require("./Link");
var route_1 = require("../route");
var Icon_1 = require("./Icon");
var Logo_1 = require("./Logo");
var AddPlaylistModal_1 = require("../pages/playlist/AddPlaylistModal");
var Container = styled_components_1.default.div(templateObject_1 || (templateObject_1 = __makeTemplateObject(["\n  display: flex;\n  justify-content: space-between;\n  background-color: ", ";\n"], ["\n  display: flex;\n  justify-content: space-between;\n  background-color: ", ";\n"])), color_1.colors.white);
var Nav = styled_components_1.default.div(templateObject_2 || (templateObject_2 = __makeTemplateObject(["\n  text-transform: uppercase;\n  display: flex;\n  align-items: center;\n  padding-right: 4%;\n  font-size: 1.1rem;\n  font-weight: bold;\n"], ["\n  text-transform: uppercase;\n  display: flex;\n  align-items: center;\n  padding-right: 4%;\n  font-size: 1.1rem;\n  font-weight: bold;\n"])));
var NavItem = styled_components_1.default.span(templateObject_3 || (templateObject_3 = __makeTemplateObject(["\n  padding: 0 10px;\n  transition: color 500ms;\n  &:hover {\n    color: ", ";\n  }\n"], ["\n  padding: 0 10px;\n  transition: color 500ms;\n  &:hover {\n    color: ", ";\n  }\n"])), color_1.colors.blue200);
var LogoWrapper = styled_components_1.default.div(templateObject_4 || (templateObject_4 = __makeTemplateObject(["\n  padding: 10px 20px;\n  padding-left: 4%;\n"], ["\n  padding: 10px 20px;\n  padding-left: 4%;\n"])));
var AddButton = styled_components_1.default(Icon_1.Icons.plusCircle)(templateObject_5 || (templateObject_5 = __makeTemplateObject(["\n  font-size: 25px;\n  padding-right: 10px;\n  color: ", ";\n  cursor: pointer;\n"], ["\n  font-size: 25px;\n  padding-right: 10px;\n  color: ", ";\n  cursor: pointer;\n"])), color_1.colors.crimson100);
exports.Header = function () {
    var user = session_1.useUser();
    var route = session_1.useRoute();
    var _a = __read(react_1.useState(false), 2), isAddPlaylistOverlayShown = _a[0], setIsAddPlaylistOverlayShown = _a[1];
    var shouldShowAddPlaylistButton = route !== route_1.routes.createPlaylist;
    return (<>
      <Container>
        <LogoWrapper>
          <Logo_1.Logo />
        </LogoWrapper>
        <Nav>
          {(function () {
        switch (user.status) {
            case http_1.DataStatus.Success:
                return (<>
                    {shouldShowAddPlaylistButton && (<AddButton onClick={function () { return setIsAddPlaylistOverlayShown(true); }}/>)}
                    <NavItem>
                      <Link_1.Link to={route_1.routes.home}>{user.username}</Link_1.Link>
                    </NavItem>
                    <NavItem>
                      <Link_1.Link to={route_1.routes.home}>Logout</Link_1.Link>
                    </NavItem>
                  </>);
            default:
                return (<>
                    <NavItem>
                      <Link_1.Link to={route_1.routes.login}>Sign In</Link_1.Link>
                    </NavItem>
                    <NavItem>
                      <Link_1.Link to={route_1.routes.register}>Register</Link_1.Link>
                    </NavItem>
                    <NavItem>
                      <Link_1.Link to={route_1.routes.about}>About</Link_1.Link>
                    </NavItem>
                  </>);
        }
    })()}
        </Nav>
      </Container>
      {isAddPlaylistOverlayShown && (<AddPlaylistModal_1.AddPlaylistModal onClose={function () { return setIsAddPlaylistOverlayShown(false); }}/>)}
    </>);
};
var templateObject_1, templateObject_2, templateObject_3, templateObject_4, templateObject_5;
