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
var http_1 = require("../http");
var endpoint_1 = require("../endpoint");
var Container_1 = require("../ui/Container");
var Button_1 = require("../ui/Button");
var Error_1 = require("../ui/Error");
var react_use_1 = require("react-use");
var react_redux_1 = require("react-redux");
var Input_1 = require("../ui/Input");
var useForm_1 = require("../ui/useForm");
var session_1 = require("../session");
var route_1 = require("../route");
var Link_1 = require("../ui/Link");
var validate_1 = require("../ui/validate");
var ValidationError;
(function (ValidationError) {
    ValidationError["UsernameMissing"] = "Please enter username";
    ValidationError["PasswordMissing"] = "Please enter password";
    ValidationError["UsernameTooShort"] = "Username must be at least 4 characters long";
    ValidationError["PasswordTooShort"] = "Password must be at least 6 characters long";
    ValidationError["None"] = "";
})(ValidationError || (ValidationError = {}));
var RegisterResponseError;
(function (RegisterResponseError) {
    RegisterResponseError["UserAlreadyExists"] = "UserAlreadyExists";
    RegisterResponseError["InvalidRequest"] = "InvalidRequest";
    RegisterResponseError["PasswordHashingFailed"] = "PasswordHashingFailed";
    RegisterResponseError["ValidationFailed"] = "ValidationFailed";
})(RegisterResponseError || (RegisterResponseError = {}));
var registerEndpoint = endpoint_1.createEndpoint('/register');
var RegisterForm = styled_components_1.default(Container_1.centered(styled_components_1.default.form))(templateObject_1 || (templateObject_1 = __makeTemplateObject(["\n  height: 66%;\n"], ["\n  height: 66%;\n"])));
var Title = styled_components_1.default.h1(templateObject_2 || (templateObject_2 = __makeTemplateObject(["\n  font-size: 3rem;\n"], ["\n  font-size: 3rem;\n"])));
var SubmitButton = styled_components_1.default(Button_1.Button).attrs({ type: 'submit' })(templateObject_3 || (templateObject_3 = __makeTemplateObject(["\n  margin-top: 10px;\n  margin-bottom: 15px;\n"], ["\n  margin-top: 10px;\n  margin-bottom: 15px;\n"])));
exports.Register = function () {
    react_use_1.useTitle('Register - Listeo');
    var dispatch = react_redux_1.useDispatch();
    var http = http_1.useHttp();
    var _a = __read(react_1.useState(http_1.remoteData.notAsked), 2), registerResponse = _a[0], setRegisterResponse = _a[1];
    var registerForm = useForm_1.useForm({
        onSubmit: function () {
            if (usernameInput.isValid && passwordInput.isValid) {
                setRegisterResponse(http_1.remoteData.loading);
                http
                    .post(registerEndpoint, {
                    username: usernameInput.value,
                    password: passwordInput.value
                })
                    .then(function (response) {
                    dispatch(session_1.session.effects.authSuccess(response.jwt));
                    setRegisterResponse(http_1.remoteData.success(response));
                })
                    .catch(function (response) {
                    setRegisterResponse(http_1.remoteData.fail(response));
                });
            }
        }
    });
    var usernameInput = Input_1.useInput({
        trim: true,
        validations: [
            validate_1.rule(validate_1.ifBlank, ValidationError.UsernameMissing),
            validate_1.rule(validate_1.ifShorterThan(4), ValidationError.UsernameTooShort)
        ],
        shouldShowError: function (_) { return registerForm.submitted; }
    });
    var passwordInput = Input_1.useInput({
        trim: true,
        validations: [
            validate_1.rule(validate_1.ifBlank, ValidationError.PasswordMissing),
            validate_1.rule(validate_1.ifShorterThan(6), ValidationError.PasswordTooShort)
        ],
        shouldShowError: function (_) { return registerForm.submitted; }
    });
    var registerRequestErrorText = (function () {
        switch (registerResponse.status) {
            case http_1.DataStatus.Fail: {
                switch (registerResponse.error) {
                    case RegisterResponseError.UserAlreadyExists:
                        return 'User already exists';
                    default:
                        return 'Something went wrong';
                }
            }
            default:
                return '';
        }
    })();
    var isSubmitButtonDisabled = registerResponse.status === http_1.DataStatus.Loading ||
        !usernameInput.isValid ||
        !passwordInput.isValid;
    return (<RegisterForm {...registerForm}>
      <Title>Register</Title>
      <Input_1.Input {...usernameInput} placeholder="Username"/>
      <Input_1.Input {...passwordInput} placeholder="Password" type="password"/>
      <SubmitButton disabled={isSubmitButtonDisabled}>Beam me up!</SubmitButton>
      <Error_1.Error visible={!!registerRequestErrorText}>
        {registerRequestErrorText}
      </Error_1.Error>
      <Link_1.Link to={route_1.routes.login}>Already have an account?</Link_1.Link>
    </RegisterForm>);
};
var templateObject_1, templateObject_2, templateObject_3;
