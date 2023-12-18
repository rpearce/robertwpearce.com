---
author: "Robert Pearce"
description: "Use React and TypeScript to build your team an accessible, shareable component library that can be included in as many projects as you can manage."
keywords: "javascript, react, react library, react component library, component library, storybook, accessibility"
title: "Build Your Team an Accessible, Shareable Component Library"
---

Today we're going to dive into building a frontend component library from start
to finish that you can share privately with your team or publicly with everyone!

By the end of this post, you will be able to use
[TypeScript](https://www.typescriptlang.org), [React](https://github.com/facebook/react),
[Storybook](https://github.com/storybookjs/storybook), and more to provide a
simple way to create accessible components that can be included in all of your
projects.

If you'd like to skip to the code, here is the example component library we're
going to make: https://github.com/rpearce/example-component-library.

## Overview

This is a big post that covers a lot of ground, so buckle up.

- [Overview](#overview)
- [When Should I Make a Component Library and Why?](#when-should-i-make-a-component-library-and-why)
  - [Scenario 1: Component Entropy](#scenario-1-component-entropy)
  - [Scenario 2: Multiple Projects (or The Possibility of Multiple Projects)](#scenario-2-multiple-projects-or-the-possibility-of-multiple-projects)
- [Project API (Usage)](#project-api-usage)
  - [JS Imports](#js-imports)
  - [CSS Imports](#css-imports)
- [Main Project Tools](#main-project-tools)
- [Project Structure](#project-structure)
- [Component Structure](#component-structure)
- [Creating the Project](#creating-the-project)
- [TypeScript Setup](#typescript-setup)
- [Linting Setup](#linting-setup)
- [Testing Setup](#testing-setup)
- [Storybook Setup](#storybook-setup)
- [An Example Component](#an-example-component)
  - [Component File](#component-file)
  - [Component CSS](#component-css)
  - [Component Tests](#component-tests)
  - [Component Stories](#component-stories)
- [Building Our TypeScript](#building-our-typescript)
- [Building Our CSS](#building-our-css)
- [Building Our Stories](#building-our-stories)
- [Continuous Integration Notes](#continuous-integration-notes)
- [Publishing Notes](#publishing-notes)
- [Wrapping Up](#wrapping-up)

## When Should I Make a Component Library and Why?

### Scenario 1: Component Entropy

Components make up large parts of our applications. As projects age, components
can become increasingly coupled with other components, business logic, and
application state management tools like [redux](https://github.com/reduxjs/redux).

These components usually start out small, focused, and pure. As time passes and
the imperative of timely code delivery takes its toll, these components become
harder to compose, harder to reason about, and cause us to yearn for simpler,
less-involved times.

Instead of rewriting those components in place and repeating the same process,
consider extracting and developing each one in isolation in a library. This will
allow you to keep each one's surface area small and keep your business logic,
state management, routing logic, etc., where it belongs: in your application.

With this scenario, a good intermediary step, before pulling components into
their own project, would be to create a folder in your application for these
components and set up a tool like storybook to house the individual examples and
compositions of them.

### Scenario 2: Multiple Projects (or The Possibility of Multiple Projects)

Consider this exchange:

> **Them**: You know that spinner/widget/dropdown/search thing we have over
> here? It looks and works great! We want the same thing over here and over
> here. How difficult is that?
>
> **Me**: Those are different projects, and that is really more like 4 different
> components working together, so a) hard to do cleanly but good for the
> long-term or b) easy (for now) if I copy and paste.
>
> **Them**: We need to ship.
>
> **Me**: Okay, so copy and paste it is...

What's special about this exchange is that _both sets of concerns and
perspectives are valid_. Software stakeholders typically want and need to ship
features and fixes quickly, and they usually want to maintain brand consistency
across their ecosystems. Software developers at those companies want to be able
to ship features and fixes and maintain brand consistency, but they are also
aware of the cost of short-term decision making (this is a way of accruing
technical debt).

We know that even the best code is useless to a business if there are no
customers around paying to use it, but we also know that suboptimal tech
decision making can grind projects to a halt over time, averting the
stakeholder's directive of shipping features and fixes quickly.

So what can we do to not only amend the scenario above but also make this
undesired state unrepresentable in the future?  We can start our projects with
an accompanying component library! For existing projects, we can begin moving
them in that direction.

## Project API (Usage)

Let's first define how we are going to include our components in our project.

### JS Imports

Component JavaScript can be imported in a few different ways:

```javascript
// import from the main (or module) specification in
// package.json, depending on your bundler and its version
import { Circle } from 'mylib'

// straight from the ESModule build
import Circle from 'mylib/dist/esm/Circle'

// straight from the CommonJS build
import Circle from 'mylib/dist/cjs/Circle'

// straight from the Universal Module Definition build
import Circle from 'mylib/dist/umd/Circle'
```

### CSS Imports

Component CSS can be imported like this:

```javascript
import 'mylib/dist/css/Circle/styles.css'
```

If you know you will use all of the components and wish to import all of their
CSS at once:

```javascript
import 'mylib/dist/css/styles.css'
```

The JS import is simple enough, but you might be wondering, "What's the deal
with importing CSS like this? I thought we were on to things like
styled-components, emotion, CSS modules, etc?"

These tools are great if the consuming application can bundle up and inject the
styles using the same instance of the tool, but can you guarantee each app will
use these same styling tools? If so, by all means go that direction. However, if
your library is injecting its own styles into the document at runtime, you will
not only potentially run into style specificity / collision issues if you don't
have the application styles load last, but strict content security policies will
potentially disallow the dynamically added styles from even being applied!

The solution? Go with the lowest common denominator: regular, vanilla CSS (or
something that outputs regular, vanilla CSS). We'll come back to this in [the
example component section](#an-example-component).

## Main Project Tools
It's time to build the project! Here are the main tools we will use:

* [NodeJS](https://nodejs.org/en/) (version `13.13.0`)
* [TypeScript](https://www.typescriptlang.org)
* [React](https://github.com/facebook/react)
* [Storybook (UI examples)](https://github.com/storybookjs/storybook)
* [jest](https://github.com/facebook/jest) (testing)
* [axe-core](https://github.com/dequelabs/axe-core) (testing accessibility)
* linting
  * [eslint](https://github.com/eslint/eslint) with [prettier](https://github.com/prettier/prettier)
  * [husky](https://www.npmjs.com/package/husky) with [lint-staged](https://www.npmjs.com/package/lint-staged); only on `pre-push`

## Project Structure

```text
.
├── .storybook            (1)
│   └── ...
├── dist                  (2)
│   └── ...
├── docs                  (3)
│   └── ...
├── examples              (4)
│   └── ...
├── scripts
│   └── buildCSS          (5)
├── source                (6)
│   └── ...
├── .eslintignore
├── .eslintrc.js
├── .gitignore
├── .prettierrc.js
├── CHANGELOG.md          (7)
├── LICENSE               (8)
├── README.md
├── husky.config.js
├── jest.config.js
├── lint-staged.config.js
├── package.json
├── testSetup.ts
├── tsconfig.base.json    (9)
├── tsconfig.cjs.json
├── tsconfig.esm.json
├── tsconfig.json
└── tsconfig.umd.json
```

1. `.storybook/` – storybook examples configuration
1. `dist/` – compiled project output
1. `docs/` – compiled storybook examples output
1. `examples/` – add `create-react-app`, `gatsby`, and other example projects here
1. `scripts/buildCSS` – store build scripts here like this CSS-related one
1. `source/` – where your project lives; we'll dive into this in the next section
1. `CHANGELOG.md` – be a good teammate and document your library's changes; very useful for your teams and useful if you decide to open source the project
1. `LICENSE` – a good idea if you plan to open source; otherwise, put `UNLICENSED` in your `package.json` license field
1. `tsconfig.json`, et al – typescript build configs; we'll dive into this in [the project setup section](#project-setup)

## Component Structure

```text
.
└── source
    └── ComponentA
        ├── __snapshots__
        │   └── test.tsx.snap
        ├── index.tsx
        ├── stories.tsx
        ├── styles.css
        └── test.tsx
    └── ComponentB
        └── ...
    └── ComponentC
        └── ...
    ├── index.ts
    └── test.tsx
```

The component and everything to do with it are co-located in the
`source/ComponentA/` folder:
* `index.tsx` component file (and any additional component files)
* storybook stories
* CSS
* tests

This grouping of everything having to do with a component makes it very easy to
find everything you need. If you would prefer a different setup, you can adjust
the tool configurations however you like.

Each component is then exported from the main `index.ts` file.

It's now time to start the project from scratch and make this outline a reality!

## Creating the Project

To begin, let's create the project and a `package.json` file with some
project-related information:

```text
$ mkdir example-component-library && cd $_
$ touch package.json
```

And in `package.json`:

```javascript
{
  "name": "@yournpm/example-component-library",
  "version": "0.1.0",
  "description": "Example repository for a shared React components library",
  "main": "dist/cjs/index.js",
  "module": "dist/esm/index.js",
  "repository": {
    "type": "git",
    "url": "git@github.com:yourgithub/example-component-library.git"
  },
  "homepage": "https://github.com/yourgithub/example-component-library",
  "bugs": "https://github.com/yourgithub/example-component-library",
  "author": "Your Name <you@youremail.com>",
  "license": "BSD-3",
  "keywords": [],
  "tags": [],
  "sideEffects": ["dist/**/*.css"],
  "files": ["LICENSE", "dist/"],
  "scripts": {},
  "devDependencies": {},
  "peerDependencies": {
    "react": "*",
    "react-dom": "*"
  },
  "dependencies": {}
}
```

Once you save that, run your build tool to make sure everything is ok:

```text
$ npm install
```

Notably, we've set our `main` field to `dist/cjs/index.js`, the CommonJS build,
for compatibility with NodeJS environments because they don't yet work well with
ESModules. We've set our `module` field to look at `dist/esm/index.js`, the
ESModule build. If you want to make use of the Universal Module Definition build
we'll create later on, you can use the `browser` field:
`"browser": "dist/umd/index.js"`. Personally, if I build with webpack, I want
webpack to select the `module` field over the `browser` one because it will
always be of a smaller size, for the UMD builds are meant to be run in any of a
few different environments.

Also of importance is the `sideEffects` field. If our library code was pure and
didn't have side effects, we would set the value to `false`, and build tools
like webpack would prune away all of the unused code. However, since we also are
exporting CSS, we need to make sure that it doesn't get dropped by the build
tool, so we do that with `"sideEffects": ["dist/**/*.css"]`.

Lastly, we know we're going to be using React, so we can go ahead and set that
as a `peerDependency` (it's up to you to decide what versions of React you'll
support).

## TypeScript Setup

We can now add TypeScript to our project with some compiler and project-related
options. We'll also add some type definition libraries that we'll use later, as
well as a dependency on [`tslib`](https://www.npmjs.com/package/tslib) to make
compiling our code to ES5 seamless.

```text
$ npm install --save-dev --save-exact \
  @types/node \
  @types/react \
  @types/react-dom \
  typescript
$ npm install --save --save-exact tslib
$ touch tsconfig.base.json tsconfig.json
```

We will place our `compilerOptions` in `tsconfig.base.json` so that they can be
extended in all our different builds in the future:

```javascript
{
  "compilerOptions": {
    "allowJs": false,
    "allowSyntheticDefaultImports": true,
    "declaration": true,
    "esModuleInterop": true,
    "importHelpers": true,
    "jsx": "react",
    "lib": ["es2020", "dom"],
    "moduleResolution": "node",
    "noImplicitAny": true,
    "outDir": "dist/",
    "sourceMap": false,
    "strict": true,
    "target": "es5"
  }
}
```

Note that the `importHelpers` flag tells `tslib` whether it should be enabled or
not.

The `tsconfig.json` will be used as a default to include our future `source`
directory:

```javascript
{
  "extends": "./tsconfig.base.json",
  "include": ["source/**/*"]
}
```

We'll add some more TypeScript-related packages when we get to the tools that
need them, and we'll add more TypeScript build configurations in the section on
[building our typescript](#building-our-typescript).

## Linting Setup

Linting is a great way to have everyone adhere to the same set of rules for code
style. For our project, we're going to install a few tools to help us out.

```text
$ npm install --save-dev --save-exact \
  @typescript-eslint/eslint-plugin \
  @typescript-eslint/parser \
  eslint \
  eslint-config-prettier \
  eslint-plugin-jest \
  eslint-plugin-jsx-a11y \
  eslint-plugin-prettier \
  eslint-plugin-react \
  eslint-plugin-react-hooks \
  husky \
  lint-staged \
  prettier
$ touch \
  .eslintignore \
  .eslintrc.js \
  .prettierrc.js \
  husky.config.js \
  lint-staged.config.js
```

The `.eslintignore` file will make sure we include files and folders that are
ignored by default (using the `!`) and exclude files and folders that we don't
care about linting.

```text
!.eslintrc.js
!.prettierrc.js
!.storybook/
dist/
docs/
examples/
```

The `.eslintrc.js` file is something you and your team will need to figure out
for yourselves, but here's where I stand on the issues:

```javascript
module.exports = {
  env: {
    browser: true,
    es6: true,
    jest: true,
    node: true,
  },
  extends: [
    'plugin:react/recommended',
    'plugin:@typescript-eslint/recommended',
    'prettier/@typescript-eslint',
    'plugin:prettier/recommended',
    'plugin:jsx-a11y/recommended',
  ],
  parserOptions: {
    ecmaVersion: 2020,
    sourceType: 'module',
  },
  parser: '@typescript-eslint/parser',
  plugins: ['jsx-a11y', 'react', 'react-hooks', '@typescript-eslint'],
  rules: {
    '@typescript-eslint/no-unused-vars': 'error',
    'jsx-quotes': ['error', 'prefer-double'],
    'jsx-a11y/no-onchange': 'off', // https://github.com/evcohen/eslint-plugin-jsx-a11y/issues/398
    'no-trailing-spaces': 'error',
    'object-curly-spacing': ['error', 'always'],
    quotes: ['error', 'single', { allowTemplateLiterals: true }],
    'react-hooks/exhaustive-deps': 'error',
    'react-hooks/rules-of-hooks': 'error',
    'react/prop-types': 'off',
    semi: ['error', 'never'],
  },
  settings: {
    react: {
      version: 'detect',
    },
  },
  overrides: [
    {
      files: ['*.js', '*.jsx'],
      rules: {
        '@typescript-eslint/explicit-function-return-type': 'off',
        '@typescript-eslint/no-var-requires': 'off',
      },
    },
  ],
}
```

The `.prettierrc.js` file defines your
[prettier](https://github.com/prettier/prettier) configuration:

```javascript
module.exports = {
  semi: false,
  singleQuote: true,
}
```

We're almost done with the linting! There are two files left.

For our `husky.config.js` file, we'll set it up to run `lint-staged` before we
push our code to our repository:

```javascript
module.exports = {
  hooks: {
    'pre-push': 'lint-staged',
  },
}
```

And for `lint-staged.config.js`, we'll specify that we want to run `eslint
--fix` on our staged files:

```javascript
module.exports = {
  '*': ['eslint --fix'],
}
```

Now that we've got this all in place, we can update our `package.json`'s
`script` object to include a `lint` command:

```javascript
"scripts": {
  "lint": "eslint ."
},
```

You can test this by running:

```text
$ npm run lint
```

## Testing Setup

We're going to use Jest and [`@testing-library/react`](https://testing-library.com/react)
to handle running our tests and testing our component code, so let's install
those tools and their companion TypeScript libraries. We'll also install
axe-core to handle some automated accessibility testing.

```text
$ npm install --save-dev --save-exact \
  @testing-library/jest-dom \
  @testing-library/react \
  @types/jest \
  axe-core \
  jest \
  ts-jest
$ touch jest.config.js testSetup.ts
```

Our `jest.config.js` collects coverage from the right places, ignores
distribution and example directories, requires the `testSetup.ts` file, and sets
us up to use TypeScript in our tests.

```javascript
module.exports = {
  clearMocks: true,
  collectCoverage: true,
  collectCoverageFrom: ['<rootDir>/source/**/*.{ts,tsx}'],
  coveragePathIgnorePatterns: [
    '/node_modules/',
    '<rootDir>/source/@types',
    'stories',
  ],
  moduleNameMapper: {},
  preset: 'ts-jest',
  setupFilesAfterEnv: ['<rootDir>/testSetup.ts'],
  testPathIgnorePatterns: ['dist/', 'examples/'],
  verbose: true,
}
```

And here is our `testSetup.ts` file that you can use to provide global testing
tools, patch JSDOM, and more:

```javascript
import '@testing-library/jest-dom/extend-expect'
```

All we do in `testSetup.ts` is add a lot of custom matchers to the `expect`
function from jest via [`@testing-library/jest-dom`](https://github.com/testing-library/jest-dom).

While we're on the testing subject, we should also update our `package.json`'s
`scripts` object to include a `test` command:

```javascript
"scripts": {
  // ...
  "test": "jest"
},
```

We don't have any test files yet, but you can confirm everything is set up
correctly by running

```text
$ npm run test
```

## Storybook Setup

Storybook is a great way to not only share examples of your components but also
get instant feedback while developing them, as well. It also comes with [a great
set of official addons](https://storybook.js.org/addons/).

Let's install Storybook for React with TypeScript, and let's also add the addons
for accessibility and knobs:

```text
$ npm install --save-dev --save-exact \
  @storybook/addon-a11y \
  @storybook/addon-knobs \
  @storybook/preset-typescript \
  @storybook/react \
  babel-loader \
  ts-loader
$ mkdir .storybook
$ touch .storybook/main.js
```

The `.storybook/main.js` file is where we can specify our Storybook options:

```javascript
module.exports = {
  addons: [
    '@storybook/addon-a11y',
    '@storybook/addon-knobs',
    '@storybook/preset-typescript',
  ],
  stories: ['../source/**/*/stories.tsx'],
}
```

## An Example Component

For our example component, we are going to make a circle with SVG. With only
this simple component, we will cover the following aspects of component
development:
* TypeScript interfaces for required and optional React props
* Component CSS
* Testing (regular, snapshot, and accessibility)
* Storybook examples

Let's create the files we know we're going to need:

```text
$ mkdir source/Circle
$ touch source/Circle/index.tsx \
  source/Circle/stories.tsx \
  source/Circle/styles.css \
  source/Circle/test.tsx
```

### Component File

```typescript
import React, { FC } from 'react'

// className, desc, and fill are optional,
// whereas title and size are required
interface Props {
  className?: string
  desc?: string
  fill?: string
  size: number
  title: string
}

// we provide our Props interface to the
// function component type
const Circle: FC<Props> = ({
  className = 'rl-circle',
  desc,
  fill,
  size,
  title,
}) => (
  <svg
    className={className}
    height={size}
    fill={fill}
    role="img"
    viewBox="0 0 100 100"
    width={size}
    xmlns="http://www.w3.org/2000/svg"
  >
    <title>{title}</title>
    {desc && <desc>{desc}</desc>}
    <circle cx="50" cy="50" r="50" />
  </svg>
)

export default Circle
```

In this component file, we define the parameters that we're willing to work
with, provide a fallback in the case of `className`, and make a regular old
component.

This file should be pretty straightforward, so let's move on to the CSS!

### Component CSS

This is a real easy one.

```css
.rl-circle { margin: 1em; }
```

The `rl` is short for "react library", and I made it up. The CSS that we are
creating needs to be made unique, and prefixing your classes is the simplest way
of doing that.

### Component Tests

It's time to write some tests! We're going to make explicit expectations and
do some snapshot tests so that everybody is happy.

```typescript
import React from 'react'
import { render } from '@testing-library/react'
import Circle from './index'

test('with all props', () => {
  const { asFragment, container, getByText } = render(
    <Circle
      className="class-override"
      desc="A blue circle"
      fill="#30336b"
      size={200}
      title="Water planet"
    />
  )
  const svgEl = container.querySelector('svg')
  const titleEl = getByText('Water planet')
  const descEl = getByText('A blue circle')

  expect(svgEl).toHaveAttribute('height', '200')
  expect(svgEl).toHaveAttribute('width', '200')
  expect(titleEl).toBeInTheDocument()
  expect(descEl).toBeInTheDocument()
  expect(asFragment()).toMatchSnapshot()
})

test('with only title & size', () => {
  const { asFragment, container, getByText } = render(
    <Circle title="Water planet" size={200} />
  )
  const svgEl = container.querySelector('svg')
  const titleEl = getByText('Water planet')
  const descEl = container.querySelector('desc')

  expect(svgEl).toHaveAttribute('height', '200')
  expect(svgEl).toHaveAttribute('width', '200')
  expect(titleEl).toBeInTheDocument()
  expect(descEl).not.toBeInTheDocument()
  expect(asFragment()).toMatchSnapshot()
})
```

These first tests provide different sets of props and test various aspects of
our component based on given props' inclusion.

Next, we can use the `axe-core` tool to try our hand at accessibility testing:

```typescript
import axe from 'axe-core'

// ...

test('is accessible with title, desc, size', (done) => {
  const { container } = render(
    <Circle desc="A blue circle" size={200} title="Water planet" />
  )

  axe.run(container, {}, (err, result) => {
    expect(err).toEqual(null)
    expect(result.violations.length).toEqual(0)
    done()
  })
})

test('is inaccessible without title', (done) => {
  const { container } = render(
    <Circle desc="A blue circle" title="Water circle" size={200} />
  )

  // do something very wrong to prove a11y testing works
  container.querySelector('title')?.remove()

  axe.run(container, {}, (err, result) => {
    expect(err).toEqual(null)
    expect(result.violations[0].id).toEqual('svg-img-alt')
    done()
  })
})
```

While the first test should be clear, the second test almost seems pointless
(hint: it is). I am including it here to demonstrate what a failing
accessibility scenario might look like. In reality, the first test in this group
pointed out the error in the second test, for I was originally _not_ requiring
`title`, but I was giving the SVG `role="img"`. This is a no-no if there is no
`aria-label`, `aria-labelledby`, nor `<title>` to supply the SVG with any
textual meaning.

Testing is easy if you keep things simple, and automated accessibility testing
is even easier than that, for all you need to do is provide DOM elements.

### Component Stories

I find it very difficult to do test driven development when developing
components, for it is an exploratory, creative experience for me. Instant
feedback makes it easy to run through all my bad ideas (there are many!) and
eventually land on some good ones. Storybook stories can help us do that, so
let's make our first story in `source/Circle/stories.tsx`.

```typescript
import React from 'react'
import { storiesOf } from '@storybook/react'
import { withA11y } from '@storybook/addon-a11y'
import { color, number, text, withKnobs } from '@storybook/addon-knobs'

// import our component and styles from
// the distribution (build) output
import { Circle } from '../../dist/esm'
import '../../dist/css/Circle/styles.css'

// group our stories under "Circle"
const stories = storiesOf('Circle', module)

// enable the accessibility & knobs addons
stories.addDecorator(withA11y)
stories.addDecorator(withKnobs)

// add a new story and use the
// knobs tools to provide named
// defaults that you can alter
// in the Storybook interface
stories.add('default', () => (
  <Circle
    desc={text('desc', 'A blue circle')}
    fill={color('fill', '#7ed6df')}
    size={number('size', 200)}
    title={text('title', 'Abstract water planet')}
  />
))

stories.add('another scenario...', () => (
  <Circle {/* other example props here */} />
))
```

Each component gets its own `stories.tsx` file, so there's no need to worry
about them getting out of hand with all the different components in your
library. Add as many different stories for your components as you like! Our
Storybook config will collect them all for you into a single place.

## Building Our TypeScript

We've already created a `tsconfig.base.json` and `tsconfig.json` file, and now
it's time to add ones for CommonJS (CJS), ESModules (ESM), and Universal Module
Definitions (UMD). We will then add some NPM scripts to build out TypeScript for
us.

```text
$ touch tsconfig.cjs.json tsconfig.esm.json tsconfig.umd.json
```

```javascript
// tsconfig.cjs.json
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "module": "commonjs",
    "outDir": "dist/cjs/"
  },
  "include": ["source/index.ts"]
}
```

```javascript
// tsconfig.esm.json
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "module": "esNext",
    "outDir": "dist/esm/"
  },
  "include": ["source/index.ts"]
}
```

```javascript
// tsconfig.umd.json
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "module": "umd",
    "outDir": "dist/umd/"
  },
  "include": ["source/index.ts"]
}
```

Each of these specify where to find the source, what type of module to output,
and where to put the resulting compiled code. If you want your code to be
compiled to the output, make sure it is either included in the `include` field
or is `require`d by something that is.

In our `package.json`, let's add some scripts that make use of these configs:

```javascript
"scripts": {
  "build:js:cjs": "tsc -p tsconfig.cjs.json",
  "build:js:esm": "tsc -p tsconfig.esm.json",
  "build:js:umd": "tsc -p tsconfig.umd.json",
  // ...
},
```

Easy! If you are guessing that we might want to run these all together in a
`build:js` command, there are two ways to do that (one verbose and one less so).

Our first attempt:

```javascript
"scripts": {
  "build:js": "npm run build:js:cjs && npm run build:js:esm && npm run build:js:umd",
  // ...
},
```

Not bad, but we can use the [`npm-run-all`](https://www.npmjs.com/package/npm-run-all)
tool to not only write a more succinct script but also run these in parallel!

```text
$ npm install --save-dev --save-exact npm-run-all
```

```javascript
"scripts": {
  "build:js": "run-p build:js:cjs build:js:esm build:js:umd",
  // ...
},
```

The `npm-run-all` tool gives us `run-p` for running scripts in parallel and
`run-s` for running them synchronously.

Watching for changes is also very simple:

```javascript
"scripts": {
  // ...
  "build:js:esm:watch": "tsc -p tsconfig.esm.json -w",
  // ...
},
```

While we're here, let's go ahead and add a `clean`ing script for our `dist/`
directory:

```javascript
"scripts": {
  // ...
  "clean": "clean:dist", // we'll add more here shortly
  "clean:dist": "rm -rf dist",
  // ...
},
```

Now that we can do some `clean`ing and `build`ing, let's create a single `build`
script that we can continue adding build steps to as we go:

```javascript
"scripts": {
  "build": "run-s clean build:js", // we'll add more here shortly
  // ...
}
```

Give it all whirl, if you like:

```text
$ npm run build
```

You should see the following tree structure for your `dist/` folder:

```text
.
└── dist
    └── cjs
        └── Circle
            ├── index.d.js
            └── index.js
        ├── index.d.js
        └── index.js
    └── esm
        └── Circle
            ├── index.d.js
            └── index.js
        ├── index.d.js
        └── index.js
    └── umd
        └── Circle
            ├── index.d.js
            └── index.js
        ├── index.d.js
        └── index.js
```

We're getting places! We have JS, and now we need our CSS.

## Building Our CSS

For our styles, we have two goals:
1. output each component's styles in a component CSS folder like `dist/css/Circle/styles.css`
1. output a combination of each component's styles in a single file in `dist/css/styles.css`

To achieve this, we're going to write a short bash script, and we're going to
place it in `scripts/buildCSS`.

```text
$ mkdir scripts
$ touch scripts/buildCSS
$ chmod +x scripts/buildCSS
```

And in `scripts/buildCSS`:

```bash
#!/bin/bash
set -euo pipefail

function copy_css {
  local dir=$(dirname $0)
  local component=$(basename $dir)
  local dist_css=$PWD/dist/css

  # concatenate component CSS to main CSS file
  mkdir -p $dist_css
  cat $0 >> $dist_css/styles.css

  # copy component CSS to component folder
  mkdir -p $dist_css/$component/
  cp $0 $dist_css/$component/
}

export -f copy_css

function build {
  find $PWD/source \
    -name '*.css' \
    -exec /bin/bash -c 'copy_css $0' {} \;
}

build
```

We lean on some `coreutils` here to solve our problems for us. The last line of
our script, `build`, calls the function of the same name that looks inside the
`source` directory for all CSS files and tells the `bash` program to run
`copy_css` with the path to the CSS file. There's a catch, though: `bash` is
going to run in a subshell, so we need to make sure our `copy_css` function is
exported and available by `export -f copy_css`.

For the `copy_css` function, it's much simpler than it looks! Here are the
steps:
1. `mkdir -p $dist_css` creates our output directory, `dist/css`.
1. `cat $0 >> $dist_css/styles.css` concatenates all the lines of our source CSS
   file and appends them to `dist/css/styles.css`.
1. `mkdir -p $dist_css/$component/` creates a component CSS folder like
   `dist/css/Circle/`. We derive the `$component` variable by getting the
   `basename` of the `dirname` of our full CSS file path. For example, `/Users/myuser/projects/example-component-library/source/Circle/styles.css`
   has a `dirname` of `/Users/rpearce/projects/example-component-library/source/Circle`,
   and that has a `basename` of `Circle`! Using that deduction, we can derive
   what component we're working with and create that output directory simply by
   finding a CSS file.
1. `cp $0 $dist_css/$component/` copies the source component CSS file to the
   output component directory; that's it!

If you have a different CSS setup, you'll need to adjust this build script
accordingly.

Now that we have our `buildCSS` script, we can add an NPM `script` to handle
building this for us and add that to our `build` script:

```javascript
"scripts": {
  "build": "run-s clean build:js build:css",
  "build:css": "./scripts/buildCSS",
  // ...
},
```

Similarly to our `build:js:esm:watch` command, how might we watch for CSS
changes and run our script in a `build:css:watch` command? Luckily, there's a
tool that can help us with that: [`chokidar`](https://www.npmjs.com/package/chokidar).

```text
$ npm install --save-dev --save-exact chokidar
```

```javascript
"scripts": {
  // ...
  "build:css:watch": "chokidar \"source/**/*.css\" -c \"./scripts/buildCSS\"",
  // ...
},
```

## Building Our Stories

To develop our components and get instant feedback in our Storybook examples,
we're going to need to run a few things at once to get it all to work together.

First, let's add a line to our `package.json`'s `scripts` object called
`storybook`:

```javascript
"scripts": {
  // ...
  "storybook": "start-storybook -p 6006"
},
```

Next, let's add a `start` command that, in this sequence,
1. cleans the `dist/` directory
1. builds only the ESModule JS output
1. builds the CSS

and then, in parallel,
1. watches the JS for changes and rebuilds the ESModule output
1. watches the CSS for changes and rebuilds the CSS
1. runs storybook, which watches for changes to the prior two items, for it will
   detect changes to its `import`s from the `dist/` folder

```javascript
"scripts": {
  // ...
  "start": "run-s clean:dist build:js:esm build:css && run-p build:js:esm:watch build:css:watch storybook",
  // ...
},
```

If you want to break those up into different scripts to make it more legible,
here's a way to do that:

```javascript
"scripts": {
  // ...
  "start": "run-s start:init start:run",
  "start:init": "run-s clean:dist build:js:esm build:css",
  "start:run": "run-p build:js:esm:watch build:css:watch storybook",
  // ...
},
```

You can then run this from the command line, and it should automatically open
your web browser and take you to http://localhost:6006.

```text
$ npm run start
```

Your Storybook library should have your component, and you can adjust the
component knobs in one of the sidebars, and you can also see the accessibility
audit located in the tab next to the knobs. _Note: no amount of automated
testing can guarantee accessibility, but it can help you catch silly mistakes._

With all these pieces in place, you can now develop your components and get
instante feedback in the browser using the same code that you would provide to a
consumer of your package!

Did you know that you can also build static HTML, CSS, and JavaScript files and
serve that up through something like GitHub Pages? We can update our
`package.json` `scripts` to include scripts for building our Storybook output
to the `docs/` folder and for cleaning the `docs/` folder, as well.

```javascript
"scripts": {
  // ...
  "build:docs": "build-storybook -o docs",
  "clean:docs": "rm -rf docs"
  "storybook": "start-storybook -p 6006"
},
```

The `clean:docs` script, if ran first, will guarantee that we have fresh output
in our `docs/` folder. Let's give it a go:

```text
$ npm run clean:docs && npm run build:docs
```

Since we can now clean and build our Storybook folder, we can update our `build`
and `clean` scripts accordingly:

```javascript
"scripts": {
  "build": "run-s clean build:js build:css build:docs",
  // ...
  "clean": "run-p clean:dist clean:docs",
  // ...
},
```

## Continuous Integration Notes

When you set up a continuous integration (CI) tool for this project, it will be
tempting to tell it to simply run `$ npm run build`; however, this will not
include your linting and testing scripts, and you could potentially have a green
light from CI when really you have problems!

While you could always run your linting and testing scripts inside of `build` (
this can get tedious) or multiple scripts from your CI configuration, let's
instead add another script named `ci` to handle this for us:

```javascript
"scripts": {
  // ...
  "ci": "run-p lint build test",
  // ...
},
```

No worries! Now we can use `$ npm run ci` in our CI configuration.

## Publishing Notes

I recommend adding a `prepublishOnly` script that ensures your linter and tests
pass before trying to build your component output:

```javascript
"scripts": {
  // ...
  "prepublishOnly": "run-p lint test && run-p build:js build:css",
  // ...
},
```

Also, if you want this to be a private repository, make sure you add
`"private": true` to your `package.json` before publishing.

## Wrapping Up

Thank you for reading this, and I hope this helps you create an awesome,
accessible component library.

Robert
