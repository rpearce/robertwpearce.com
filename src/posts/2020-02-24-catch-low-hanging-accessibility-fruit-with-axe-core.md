---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "Automate away easy-to-catch accessibility issues"
image: "/images/near-milford-sound.jpg"
keywords: "javascript, accessibility, a11y, testing, axe-core"
photoCredit: "emilycouldmakethat"
photoWebsite: "https://www.instagram.com/emilycouldmakethat"
title: "Catch Low-Hanging Accessibility Fruit with axe-core"
---

[Deque Systems](https://www.deque.com), in addition to having [an
accessibility-testing browser plugin](https://www.deque.com/axe/axe-for-web/),
has an open source package, [axe-core](https://www.npmjs.com/package/axe-core),
that can help you determine if HTML contains obvious accessibility issues.

That tool can be used on its own in your tests, or you can turn it into a
`Promise` and use it like this!

```javascript
import axe from 'axe-core'

const isA11y = html =>
  new Promise((resolve, reject) => {
    axe.run(html, {}, (err, result={}) => {
      const { violations=[] } = result

      if (err) {
        reject(err)
      } else if (violations.length > 0) {
        reject(violations)
      } else {
        // Uncomment to view incomplete/unavailable tests & why
        //console.log(result.incomplete)
        resolve(true)
      }
    })
  })

test('bad form', async () => {
  const wrap = document.createElement('div')
  wrap.innerHTML = `
    <form>
      <div>Enter your name</div>
      <input type="text" />
      <button type="submit">Submit</button>
    </form>
  `
  document.body.appendChild(wrap)

  expect(await isA11y(wrap)).toEqual(true)
})
// Failed: Array [
//   Object {
//     "description": "Ensures every form element has a label",
//     "help": "Form elements must have labels",
//     "helpUrl": "https://dequeuniversity.com/rules/axe/3.5/label?application=axeAPI",
//     "id": "label",
//     "impact": "critical",
//     "nodes": Array [
//       [Object],
//     ],
//     "tags": Array [
//       "cat.forms",
//       "wcag2a",
//       "wcag332",
//       "wcag131",
//       "section508",
//       "section508.22.n"
//     ],
//   }
//  ]
```

It can detect all sorts of accessibility issues, so long as the environment in
which it's being tested supports the browser features used in `axe-core`'s
tests. For example, `jsdom`, which `jest` uses as its browser mocking engine,
[only recently added some support for `Range`](https://github.com/jsdom/jsdom/pull/2719),
it seems there are still some aspects missing, and this prevents `axe-core` from
being able to test things like the accessibility of text color on certain
backgrounds.

That said, the sheer number of issues that can be caught with this tool is
staggering. If you work with tools like React and combine this with Deque's
[`react-axe`](https://github.com/dequelabs/react-axe) tool and
[`eslint-plugin-jsx-a11y`](https://github.com/evcohen/eslint-plugin-jsx-a11y),
you are sure to catch heaps of issues you might accidentally overlook. Note,
however, that these tools are not replacements for real accessibility testing.

Here is an example in a real OSS project of mine that uses this `axe-core`
technique with `@testing-library/react`:
https://github.com/rpearce/react-medium-image-zoom/blob/6721f87370d968361d9d0d14cd30d752832877d1/__tests__/Uncontrolled.js#L27.

* * *

If you are using `jest` and want a custom matcher, there is a project,
[`jest-axe`](https://github.com/nickcolley/jest-axe), that allows you to do so:

```javascript
// from https://github.com/nickcolley/jest-axe#usage
const { axe, toHaveNoViolations } = require('jest-axe')

expect.extend(toHaveNoViolations)

it('should demonstrate this matcher`s usage', async () => {
  const render = () => '<img src="#"/>'

  // pass anything that outputs html to axe
  const html = render()

  expect(await axe(html)).toHaveNoViolations()
})
```

* * *

Thank you for reading!
<br />
Robert
