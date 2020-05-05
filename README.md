# Disclaimer
All struct and function in this crate -- crate those based on real crate -- are entirely fictional. 
All celebrity code are impersonated.
The following program contains coarse language and due to its content it should not be used by anyone.

# Yarte [![Latest version](https://img.shields.io/crates/v/yarte.svg)](https://crates.io/crates/yarte) [![Build Status](https://travis-ci.org/botika/yarte.svg?branch=master)](https://travis-ci.org/botika/yarte) [![Financial Contributors on Open Collective](https://opencollective.com/yarte/all/badge.svg?label=financial+contributors)](https://opencollective.com/yarte)
Yarte stands for **Y**et **A**nother **R**ust **T**emplate **E**ngine, 
is the fastest template engine. Uses a Handlebars-like syntax, 
well known and intuitive for most developers. Yarte is an optimized, and easy-to-use 
rust crate, with which developers can create logic around their 
HTML templates using conditionals, loops, rust code and template composition. 

## Features
- Meta programming system around template with Rust expressions, conditionals or loops
- Recursion in partial at compile time
- No allocation helpers
- A [fancy-text debug](https://asciinema.org/a/WEY4Hu17p8qn51b5DEpBVqLL1?autoplay=1) mode to visualize the code generated by Yarte
- Emit snipped annotations of the template at error
- Mode `server` for wasm app isomorphic server side

#### Is it really the fastest?
 See it for yourself in the [benchmarks][bench]!

## Documentation
In order to  fully understand Yarte's capabilities take a look at the following documentation:
- [Tests](./yarte/tests)
- [Our book](https://yarte.netlify.com/)
- [Crate documentation](https://docs.rs/yarte/)
- Minimum supported Rust version: 1.42 or later

## Roadmap
- [ ] [#37](https://github.com/botika/yarte/issues/37) Derive for compiled DOM, implement [`App`](./yarte_wasm_app/src/lib.rs) trait 
- [ ] Minify Css
- [ ] intellij plugin (help wanted)
- [ ] yarte/html/css UI like [sciter](https://sciter.com/)
- [ ] One derive to rule them all, one derive to find them, one derive to bring them all and in the darkness bind them 
- [ ] ... you can open a issue!

We are not looking for anything other than **render HTML** and text as fast as possible

## Acknowledgment
Yarte is based on all previous templates engines, syntax as well as its documentation 
is highly influenced by [Handlebars][handlebars]. As many ideas as possible used in 
Yarte are from other repositories. 

Comments in the code clarify which ideas are used, and from where.

[bench]: https://github.com/botika/template-bench-rs#results
[handlebars]: https://handlebarsjs.com/ 

## Contributing

Please, contribute to Yarte! The more the better! Feel free to to open an issue and/or contacting directly with the 
owner for any request or suggestion.

### Code of conduct
This Code of Conduct is adapted from the [Contributor Covenant][homepage], version 1.4, available at [http://contributor-covenant.org/version/1/4][version]

[homepage]: http://contributor-covenant.org
[version]: http://contributor-covenant.org/version/1/4/

## Contributors

### Code Contributors

This project exists thanks to all the people who contribute.
<a href="https://github.com/botika/yarte/graphs/contributors"><img src="https://opencollective.com/yarte/contributors.svg?width=890&button=false" /></a>

### Financial Contributors

Become a financial contributor and help us sustain our community. [[Contribute](https://opencollective.com/yarte/contribute)]

#### Individuals

<a href="https://opencollective.com/yarte"><img src="https://opencollective.com/yarte/individuals.svg?width=890"></a>

#### Organizations

Support this project with your organization. Your logo will show up here with a link to your website. [[Contribute](https://opencollective.com/yarte/contribute)]

<a href="https://opencollective.com/yarte/organization/0/website"><img src="https://opencollective.com/yarte/organization/0/avatar.svg"></a>
<a href="https://opencollective.com/yarte/organization/1/website"><img src="https://opencollective.com/yarte/organization/1/avatar.svg"></a>
<a href="https://opencollective.com/yarte/organization/2/website"><img src="https://opencollective.com/yarte/organization/2/avatar.svg"></a>
<a href="https://opencollective.com/yarte/organization/3/website"><img src="https://opencollective.com/yarte/organization/3/avatar.svg"></a>
<a href="https://opencollective.com/yarte/organization/4/website"><img src="https://opencollective.com/yarte/organization/4/avatar.svg"></a>
<a href="https://opencollective.com/yarte/organization/5/website"><img src="https://opencollective.com/yarte/organization/5/avatar.svg"></a>
<a href="https://opencollective.com/yarte/organization/6/website"><img src="https://opencollective.com/yarte/organization/6/avatar.svg"></a>
<a href="https://opencollective.com/yarte/organization/7/website"><img src="https://opencollective.com/yarte/organization/7/avatar.svg"></a>
<a href="https://opencollective.com/yarte/organization/8/website"><img src="https://opencollective.com/yarte/organization/8/avatar.svg"></a>
<a href="https://opencollective.com/yarte/organization/9/website"><img src="https://opencollective.com/yarte/organization/9/avatar.svg"></a>
### License
This project is distributed under the terms of both the Apache License (Version 2.0) and the MIT license, specified in 
[LICENSE-APACHE](LICENSE-APACHE) and [LICENSE-MIT](LICENSE-MIT) respectively.
