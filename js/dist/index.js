"use strict";
;
(function () {
    var state = {
        isActive: false
    };
    var cn = {
        dark: "dark",
        light: "light",
        toggle: "toggle",
        toggleDay: "toggle__day",
        toggleNight: "toggle__night",
        toggleOff: "toggle--off",
        toggleOn: "toggle--on"
    };
    var updateBodyClass = function () {
        document.body.classList.replace(state.isActive ? cn.light : cn.dark, state.isActive ? cn.dark : cn.light);
    };
    var createToggleNightBtn = function () {
        var toggleBtn = document.createElement("button");
        toggleBtn.type = "button";
        toggleBtn.setAttribute("role", "switch");
        toggleBtn.setAttribute("aria-checked", String(state.isActive));
        toggleBtn.className = cn.toggle + " " + (state.isActive ? cn.toggleOn : cn.toggleOff);
        toggleBtn.addEventListener("click", function () {
            var isActive = !state.isActive;
            state.isActive = isActive;
            localStorage.setItem("isActive", String(isActive));
            updateBodyClass();
            toggleBtn.setAttribute("aria-checked", String(isActive));
            toggleBtn.className = cn.toggle + " " + (isActive ? cn.toggleOn : cn.toggleOff);
        });
        var dayEl = document.createElement("span");
        dayEl.innerText = "\uD83C\uDF1E";
        dayEl.className = cn.toggleDay;
        dayEl.setAttribute("aria-label", "Day Mode");
        var nightEl = document.createElement("span");
        nightEl.innerText = "\uD83C\uDF19";
        nightEl.className = cn.toggleNight;
        nightEl.setAttribute("aria-label", "Night Mode");
        toggleBtn.appendChild(dayEl);
        toggleBtn.appendChild(nightEl);
        return toggleBtn;
    };
    var init = function () {
        state.isActive = window.localStorage.getItem("isActive") === "true";
        updateBodyClass();
        var el = document.querySelector("[data-nav-wrap]") || document.body;
        if (el) {
            el.appendChild(createToggleNightBtn());
        }
    };
    init();
})();
