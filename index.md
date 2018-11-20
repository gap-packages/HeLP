---
layout: default
---

# GAP Package {{site.data.package.name}}

{{site.data.package.abstract}}

The current version of this package is version {{site.data.package.version}}, released on {{site.data.package.date}}.
For more information, please refer to [the package manual]({{site.data.package.doc-html}}).
There is also a [README](README.html) file.

A short article outlining the theory and algorithms is available [here](https://msp.org/jsag/2018/8-1/p01.xhtml) or in the [ArXiv version](https://arxiv.org/abs/1507.08174). 


## Installation

Download and unpack the archive provided below in the pkg subdirectory of your GAP installation. This completes the installation of the HeLP package. Note however that this package requires the 4ti2Interface, which in turn uses the package IO. The latter package needs a C-part to be compiled; see the readme-file or the documentation of that package. The HeLP-package also requires the GAP-package NormalizInterface which also has to be build before it can be loaded in GAP; see the readme-file or the documentation of that package. This package also makes use of the software Normaliz and, if installed, 4ti2 and lrslib. The first one should automatically be installed when building the NormalizInterface, the second and the third one need to be installed manually as described homepages of the packages.
In case you have problems getting the HeLP-package running, see Section 6.1 of the manual for some trouble shooting. If this does not help, feel free to contact one of the maintainers of the package. 

## Dependencies

This package requires GAP version {{site.data.package.GAP}}
{% if site.data.package.needed-pkgs %}
The following other GAP packages are needed:
{% for pkg in site.data.package.needed-pkgs %}
- {% if pkg.url %}<a href="{{ pkg.url }}">{{ pkg.name }}</a> {% else %}{{ pkg.name }} {% endif %}
  {{- pkg.version -}}
{% endfor %}
{% endif %}
{% if site.data.package.suggested-pkgs %}
The following additional GAP packages are not required, but suggested:
{% for pkg in site.data.package.suggested-pkgs %}
- {% if pkg.url %}<a href="{{ pkg.url }}">{{ pkg.name }}</a> {% else %}{{ pkg.name }} {% endif %}
  {{- pkg.version -}}
{% endfor %}
{% endif %}


## Author{% if site.data.package.authors.size != 1 %}s{% endif %}
{% for person in site.data.package.authors %}
 {% if person.url %}<a href="{{ person.url }}">{{ person.name }}</a>{% else %}{{ person.name }}{% endif %}
 {%- if forloop.last -%}.{% else %}, {%- endif -%}
{% endfor %}

{% if site.data.package.contributors and site.data.package.contributors.size > 0 %}
## Contributor{% if site.data.package.contributors.size != 1 %}s{% endif %}
 {% for person in site.data.package.contributors %}
  {% if person.url %}<a href="{{ person.url }}">{{ person.name }}</a>{% else %}{{ person.name }}{% endif %}
  {%- if forloop.last -%}.{% else %}, {%- endif -%}
 {% endfor %}
{% endif %}

{% if site.data.package.citeas %}
## Citing

Please, cite this package as

{{site.data.package.citeas}}

You can get more info by typing `Cite("{{ site.data.package.name }}");` in the gap prompt.

{% include button-bibtex.html %}

{% endif %}

We would be grateful to hear if you are using this package. Just send an email to one of the maintainers. Comments and suggestion for improvements are very welcome. 


## Acknowledgments

The development of HeLP was partially supported by the DFG priority program SPP 1489 Algorithmic and Experimental Methods in Algebra, Geometry, and Number Theory and the Research Foundation Flanders (FWO - Vlaanderen).


{% if site.github.issues_url %}
## Feedback

For bug reports, feature requests and suggestions, please use the
[issue tracker]({{site.github.issues_url}}).
{% endif %}
