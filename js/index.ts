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
    if (state.isActive) {
      document.body.classList.add(`dark`)
      document.body.classList.remove(`light`)
    } else {
      document.body.classList.add(`light`)
      document.body.classList.remove(`dark`)
    }
    //document.body.classList.replace(
    //  state.isActive ? cn.dark : cn.light,
    //  state.isActive ? cn.light : cn.dark,
    //)
  }

  const setState = ({ isActive }) => {
    localStorage.setItem(`isActive`, isActive)
    state.isActive = isActive
    updateBodyClass()
  }

  const createToggleNightBtn = () => {
    const toggleBtn = document.createElement(`button`)

    toggleBtn.type = `button`
    toggleBtn.setAttribute(`aria-checked`, String(state.isActive))
    toggleBtn.className = `${cn.toggle} ${state.isActive ? cn.toggleOn : cn.toggleOff}`

    toggleBtn.addEventListener(`click`, () => {
      setState({ isActive: !state.isActive })

      toggleBtn.setAttribute(`aria-checked`, String(state.isActive))
      toggleBtn.className = `${cn.toggle} ${state.isActive ? cn.toggleOn : cn.toggleOff}`
    })

    const dayEl = document.createElement(`span`)
    dayEl.innerText = `🌞 Day`
    dayEl.className = cn.toggleDay

    const nightEl = document.createElement(`span`)
    nightEl.innerText = `🌙 Night`
    nightEl.className = cn.toggleNight

    toggleBtn.appendChild(dayEl)
    toggleBtn.appendChild(nightEl)

    return toggleBtn
  }

  const init = () => {
    updateBodyClass()

    const toggleNightBtn = createToggleNightBtn()
    document.body.appendChild(toggleNightBtn)
  }

  init()
})()
