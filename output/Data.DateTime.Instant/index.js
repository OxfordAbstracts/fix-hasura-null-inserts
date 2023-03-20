// Generated by purs version 0.15.7
import * as $foreign from "./foreign.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Bounded from "../Data.Bounded/index.js";
import * as Data_Date from "../Data.Date/index.js";
import * as Data_Date_Component from "../Data.Date.Component/index.js";
import * as Data_DateTime from "../Data.DateTime/index.js";
import * as Data_Enum from "../Data.Enum/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Time from "../Data.Time/index.js";
import * as Data_Time_Component from "../Data.Time.Component/index.js";
import * as Data_Time_Duration from "../Data.Time.Duration/index.js";
var fromJust = /* #__PURE__ */ Data_Maybe.fromJust();
var toEnum = /* #__PURE__ */ Data_Enum.toEnum(Data_Date_Component.boundedEnumMonth);
var show = /* #__PURE__ */ Data_Show.show(Data_Time_Duration.showMilliseconds);
var fromEnum = /* #__PURE__ */ Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth);
var bottom = /* #__PURE__ */ Data_Bounded.bottom(Data_Time_Component.boundedHour);
var bottom1 = /* #__PURE__ */ Data_Bounded.bottom(Data_Time_Component.boundedMinute);
var bottom2 = /* #__PURE__ */ Data_Bounded.bottom(Data_Time_Component.boundedSecond);
var bottom3 = /* #__PURE__ */ Data_Bounded.bottom(Data_Time_Component.boundedMillisecond);
var append1 = /* #__PURE__ */ Data_Semigroup.append(Data_Time_Duration.semigroupMilliseconds);
var negateDuration = /* #__PURE__ */ Data_Time_Duration.negateDuration(Data_Time_Duration.durationMilliseconds);
var Instant = function (x) {
    return x;
};
var unInstant = function (v) {
    return v;
};
var toDateTime = /* #__PURE__ */ (function () {
    var mkDateTime = function (y) {
        return function (mo) {
            return function (d) {
                return function (h) {
                    return function (mi) {
                        return function (s) {
                            return function (ms) {
                                return new Data_DateTime.DateTime(Data_Date.canonicalDate(y)(fromJust(toEnum(mo)))(d), new Data_Time.Time(h, mi, s, ms));
                            };
                        };
                    };
                };
            };
        };
    };
    return $foreign.toDateTimeImpl(mkDateTime);
})();
var showInstant = {
    show: function (v) {
        return "(Instant " + (show(v) + ")");
    }
};
var ordDateTime = Data_Time_Duration.ordMilliseconds;
var instant = function (v) {
    if (v >= -8.6399778816e15 && v <= 8.639977881599999e15) {
        return new Data_Maybe.Just(v);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.DateTime.Instant (line 44, column 1 - line 44, column 41): " + [ v.constructor.name ]);
};
var fromDateTime = function (v) {
    return $foreign.fromDateTimeImpl(Data_Date.year(v.value0), fromEnum(Data_Date.month(v.value0)), Data_Date.day(v.value0), Data_Time.hour(v.value1), Data_Time.minute(v.value1), Data_Time.second(v.value1), Data_Time.millisecond(v.value1));
};
var fromDate = function (d) {
    return $foreign.fromDateTimeImpl(Data_Date.year(d), fromEnum(Data_Date.month(d)), Data_Date.day(d), bottom, bottom1, bottom2, bottom3);
};
var eqDateTime = Data_Time_Duration.eqMilliseconds;
var diff = function (dictDuration) {
    var toDuration = Data_Time_Duration.toDuration(dictDuration);
    return function (dt1) {
        return function (dt2) {
            return toDuration(append1(unInstant(dt1))(negateDuration(unInstant(dt2))));
        };
    };
};
var boundedInstant = /* #__PURE__ */ (function () {
    return {
        bottom: -8.6399778816e15,
        top: 8.639977881599999e15,
        Ord0: function () {
            return ordDateTime;
        }
    };
})();
export {
    instant,
    unInstant,
    fromDateTime,
    fromDate,
    toDateTime,
    diff,
    eqDateTime,
    ordDateTime,
    boundedInstant,
    showInstant
};