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
var react_use_1 = require("react-use");
var Container_1 = require("../ui/Container");
var Button_1 = require("../ui/Button");
var Input_1 = require("../ui/Input");
var useForm_1 = require("../ui/useForm");
var route_1 = require("../route");
var Link_1 = require("../ui/Link");
var Error_1 = require("../ui/Error");
var endpoint_1 = require("../endpoint");
var http_1 = require("../http");
var react_redux_1 = require("react-redux");
var session_1 = require("../session");
var validate_1 = require("../ui/validate");
var ValidationError;
(function (ValidationError) {
    ValidationError["UsernameMissing"] = "Please enter username";
    ValidationError["PasswordMissing"] = "Please enter password";
    ValidationError["None"] = "";
})(ValidationError || (ValidationError = {}));
var LoginResponseError;
(function (LoginResponseError) {
    LoginResponseError["UserNotFound"] = "UserNotFound";
    LoginResponseError["InvalidRequest"] = "InvalidRequest";
    LoginResponseError["ServerError"] = "ServerError";
})(LoginResponseError || (LoginResponseError = {}));
var loginEndpoint = endpoint_1.createEndpoint('/login');
var LoginForm = styled_components_1.default(Container_1.centered(styled_components_1.default.form))(templateObject_1 || (templateObject_1 = __makeTemplateObject(["\n  height: 66%;\n"], ["\n  height: 66%;\n"])));
var Title = styled_components_1.default.h1(templateObject_2 || (templateObject_2 = __makeTemplateObject(["\n  font-size: 3rem;\n"], ["\n  font-size: 3rem;\n"])));
var SubmitButton = styled_components_1.default(Button_1.Button).attrs({ type: 'submit' })(templateObject_3 || (templateObject_3 = __makeTemplateObject(["\n  margin-top: 10px;\n  margin-bottom: 15px;\n"], ["\n  margin-top: 10px;\n  margin-bottom: 15px;\n"])));
exports.Login = function () {
    react_use_1.useTitle('Login - Listeo');
    var dispatch = react_redux_1.useDispatch();
    var http = http_1.useHttp();
    var _a = __read(react_1.useState(http_1.remoteData.notAsked), 2), loginResponse = _a[0], setLoginResponse = _a[1];
    var loginForm = useForm_1.useForm({
        onSubmit: function () {
            if (usernameInput.isValid && passwordInput.isValid) {
                http
                    .post(loginEndpoint, {
                    username: usernameInput.value,
                    password: passwordInput.value
                })
                    .then(function (response) {
                    dispatch(session_1.session.effects.authSuccess(response.jwt));
                    setLoginResponse(http_1.remoteData.success(response));
                })
                    .catch(function (response) {
                    setLoginResponse(http_1.remoteData.fail(response));
                });
            }
        }
    });
    var usernameInput = Input_1.useInput({
        trim: true,
        validations: [validate_1.rule(validate_1.ifBlank, ValidationError.UsernameMissing)],
        shouldShowError: function (_) { return loginForm.submitted; }
    });
    var passwordInput = Input_1.useInput({
        trim: true,
        validations: [validate_1.rule(validate_1.ifBlank, ValidationError.PasswordMissing)],
        shouldShowError: function (_) { return loginForm.submitted; }
    });
    var loginRequestErrorText = (function () {
        switch (loginResponse.status) {
            case http_1.DataStatus.Fail: {
                switch (loginResponse.error) {
                    case LoginResponseError.UserNotFound:
                        return 'User not found';
                    default:
                        return 'Something went wrong';
                }
            }
            default:
                return '';
        }
    })();
    var isSubmitButtonDisabled = loginResponse.status === http_1.DataStatus.Loading ||
        !usernameInput.isValid ||
        !passwordInput.isValid;
    return (<LoginForm {...loginForm}>
      <Title>Sign In</Title>
      <Input_1.Input {...usernameInput} placeholder="Username"/>
      <Input_1.Input {...passwordInput} placeholder="Password" type="password"/>
      <SubmitButton disabled={isSubmitButtonDisabled}>Let's go</SubmitButton>
      <Error_1.Error visible={!!loginRequestErrorText}>{loginRequestErrorText}</Error_1.Error>
      <Link_1.Link to={route_1.routes.register}>Don't have an account?</Link_1.Link>
    </LoginForm>);
};
var templateObject_1, templateObject_2, templateObject_3;
