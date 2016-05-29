"use strict";

module.exports = {
    stringify: function(name) {
        return function () {
            return `${name}(${[...arguments].join(', ')})`;
        };
    },

    assert: function(msg, expected, actual) {
        function eq(xs, ys) {
            // Check for nulls
            if (xs === null && ys === null) {
                return true;
            } else if (xs === null || ys == null) {
                return false;
            }

            // Recurse into arrays
            if (typeof xs === 'object' && typeof ys === 'object') {
                const xsIsArray = xs.constructor === Array;
                const ysIsArray = ys.constructor === Array;

                if (xsIsArray && ysIsArray) {
                    return xs.every((x, i) => eq(x, ys[i]));
                } else if (xsIsArray || ysIsArray) {
                    return false;
                }
            }

            // Catch all case
            return xs === ys;
        }

        if (eq(expected, actual)) {
            console.log("\u2714", msg);
        } else {
            console.log("\u2717", msg);
            console.log("  Expected:", expected);
            console.log("  Actual:  ", actual);
        }
    }
};
