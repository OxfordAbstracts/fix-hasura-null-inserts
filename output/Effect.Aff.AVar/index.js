// Generated by purs version 0.15.7
import * as Effect_AVar from "../Effect.AVar/index.js";
import * as Effect_Aff from "../Effect.Aff/index.js";
import * as Effect_Class from "../Effect.Class/index.js";
var liftEffect = /* #__PURE__ */ Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
var tryTake = function ($4) {
    return liftEffect(Effect_AVar.tryTake($4));
};
var tryRead = function ($5) {
    return liftEffect(Effect_AVar.tryRead($5));
};
var tryPut = function (value) {
    var $6 = Effect_AVar.tryPut(value);
    return function ($7) {
        return liftEffect($6($7));
    };
};
var take = function (avar) {
    return Effect_Aff.makeAff(function (k) {
        return function __do() {
            var c = Effect_AVar.take(avar)(k)();
            return Effect_Aff.effectCanceler(c);
        };
    });
};
var status = function ($8) {
    return liftEffect(Effect_AVar.status($8));
};
var read = function (avar) {
    return Effect_Aff.makeAff(function (k) {
        return function __do() {
            var c = Effect_AVar.read(avar)(k)();
            return Effect_Aff.effectCanceler(c);
        };
    });
};
var put = function (value) {
    return function (avar) {
        return Effect_Aff.makeAff(function (k) {
            return function __do() {
                var c = Effect_AVar.put(value)(avar)(k)();
                return Effect_Aff.effectCanceler(c);
            };
        });
    };
};
var $$new = function ($9) {
    return liftEffect(Effect_AVar["new"]($9));
};
var kill = function (error) {
    var $10 = Effect_AVar.kill(error);
    return function ($11) {
        return liftEffect($10($11));
    };
};
var empty = /* #__PURE__ */ liftEffect(Effect_AVar.empty);
export {
    $$new as new,
    empty,
    status,
    take,
    tryTake,
    put,
    tryPut,
    read,
    tryRead,
    kill
};
export {
    Empty,
    Filled,
    Killed,
    isEmpty,
    isFilled,
    isKilled
} from "../Effect.AVar/index.js";