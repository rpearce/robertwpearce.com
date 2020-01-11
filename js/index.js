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
        toggleOn: "toggle--on"
    };
    var updateBodyClass = function () {
        if (state.isActive) {
            document.body.classList.add("dark");
            document.body.classList.remove("light");
        }
        else {
            document.body.classList.add("light");
            document.body.classList.remove("dark");
        }
        //document.body.classList.replace(
        //  state.isActive ? cn.dark : cn.light,
        //  state.isActive ? cn.light : cn.dark,
        //)
    };
    var setState = function (_a) {
        var isActive = _a.isActive;
        localStorage.setItem("isActive", isActive);
        state.isActive = isActive;
        updateBodyClass();
    };
    var createToggleNightBtn = function () {
        var toggleBtn = document.createElement("button");
        toggleBtn.type = "button";
        toggleBtn.setAttribute("aria-checked", String(state.isActive));
        toggleBtn.className = cn.toggle + " " + (state.isActive ? cn.toggleOn : cn.toggleOff);
        toggleBtn.addEventListener("click", function () {
            setState({ isActive: !state.isActive });
            toggleBtn.setAttribute("aria-checked", String(state.isActive));
            toggleBtn.className = cn.toggle + " " + (state.isActive ? cn.toggleOn : cn.toggleOff);
        });
        var dayEl = document.createElement("span");
        dayEl.innerText = "\uD83C\uDF1E Day";
        dayEl.className = cn.toggleDay;
        var nightEl = document.createElement("span");
        nightEl.innerText = "\uD83C\uDF19 Night";
        nightEl.className = cn.toggleNight;
        toggleBtn.appendChild(dayEl);
        toggleBtn.appendChild(nightEl);
        return toggleBtn;
    };
    var init = function () {
        updateBodyClass();
        var toggleNightBtn = createToggleNightBtn();
        document.body.appendChild(toggleNightBtn);
    };
    init();
})();
