"use strict";
(function () {
    var state = {
        isActive: localStorage.getItem("isActive") === "true"
    };
    var cn = {
        dark: "dark",
        light: "light",
        toggle: "toggle",
        toggleDay: "toggle__day",
        toggleNight: "toggle__night",
        toggleOff: "toggle--off",
        toggleOn: "toggle--on",
    };
    var updateBodyClass = function () {
        document.body.classList.replace(state.isActive ? cn.light : cn.dark, state.isActive ? cn.dark : cn.light);
    };
    var createToggleNightBtn = function () {
        var toggleBtn = document.createElement("button");
        toggleBtn.type = "button";
        toggleBtn.setAttribute("aria-checked", String(state.isActive));
        toggleBtn.className = cn.toggle + " " + (state.isActive ? cn.toggleOn : cn.toggleOff);
        toggleBtn.addEventListener("click", function () {
            var _isActive = !state.isActive;
            state.isActive = _isActive;
            localStorage.setItem("isActive", String(_isActive));
            updateBodyClass();
            toggleBtn.setAttribute("aria-checked", String(_isActive));
            toggleBtn.className = cn.toggle + " " + (_isActive ? cn.toggleOn : cn.toggleOff);
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
        updateBodyClass();
        document.body.appendChild(createToggleNightBtn());
    };
    init();
})();
