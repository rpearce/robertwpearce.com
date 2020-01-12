(() => {
  let state = {
    isActive: localStorage.getItem(`isActive`) === `true`
  }

  const cn = {
    dark: `dark`,
    light: `light`,
    toggle: `toggle`,
    toggleDay: `toggle__day`,
    toggleNight: `toggle__night`,
    toggleOff: `toggle--off`,
    toggleOn: `toggle--on`,
  }

  const updateBodyClass = () => {
    document.body.classList.replace(
      state.isActive ? cn.light : cn.dark,
      state.isActive ? cn.dark : cn.light,
    )
  }

  const createToggleNightBtn = () => {
    const toggleBtn = document.createElement(`button`)

    toggleBtn.type = `button`
    toggleBtn.setAttribute(`aria-checked`, String(state.isActive))
    toggleBtn.className = `${cn.toggle} ${state.isActive ? cn.toggleOn : cn.toggleOff}`

    toggleBtn.addEventListener(`click`, () => {
      const _isActive = !state.isActive
      state.isActive = _isActive

      localStorage.setItem(`isActive`, String(_isActive))
      updateBodyClass()

      toggleBtn.setAttribute(`aria-checked`, String(_isActive))
      toggleBtn.className = `${cn.toggle} ${_isActive ? cn.toggleOn : cn.toggleOff}`
    })

    const dayEl = document.createElement(`span`)
    dayEl.innerText = `🌞`
    dayEl.className = cn.toggleDay
    dayEl.setAttribute(`aria-label`, `Day Mode`)

    const nightEl = document.createElement(`span`)
    nightEl.innerText = `🌙`
    nightEl.className = cn.toggleNight
    nightEl.setAttribute(`aria-label`, `Night Mode`)

    toggleBtn.appendChild(dayEl)
    toggleBtn.appendChild(nightEl)

    return toggleBtn
  }

  const init = () => {
    updateBodyClass()
    document.body.appendChild(createToggleNightBtn())
  }

  init()
})()
