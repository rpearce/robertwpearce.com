;((): void => {
  interface State {
    isActive: boolean
  }

  interface Classes {
    dark: string
    light: string
    toggle: string
    toggleDay: string
    toggleNight: string
    toggleOff: string
    toggleOn: string
  }

  const state: State = {
    isActive: false,
  }

  const cn: Classes = {
    dark: `dark`,
    light: `light`,
    toggle: `toggle`,
    toggleDay: `toggle__day`,
    toggleNight: `toggle__night`,
    toggleOff: `toggle--off`,
    toggleOn: `toggle--on`,
  }

  const updateBodyClass = (): void => {
    document.body.classList.replace(
      state.isActive ? cn.light : cn.dark,
      state.isActive ? cn.dark : cn.light
    )
  }

  const createToggleNightBtn = (): HTMLElement => {
    const toggleBtn = document.createElement(`button`)

    toggleBtn.type = `button`
    toggleBtn.setAttribute(`role`, `switch`)
    toggleBtn.setAttribute(`aria-checked`, String(state.isActive))
    toggleBtn.className = `${cn.toggle} ${
      state.isActive ? cn.toggleOn : cn.toggleOff
    }`

    toggleBtn.addEventListener(`click`, () => {
      const isActive = !state.isActive
      state.isActive = isActive

      localStorage.setItem(`isActive`, String(isActive))
      updateBodyClass()

      toggleBtn.setAttribute(`aria-checked`, String(isActive))
      toggleBtn.className = `${cn.toggle} ${
        isActive ? cn.toggleOn : cn.toggleOff
      }`
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

  const init = (): void => {
    state.isActive = window.localStorage.getItem(`isActive`) === `true`

    updateBodyClass()

    const el = document.querySelector(`[data-nav-wrap]`) || document.body

    if (el) {
      el.appendChild(createToggleNightBtn())
    }
  }

  init()
})()
