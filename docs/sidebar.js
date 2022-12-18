module.exports = {
    oldTableOfContent: [
        {
            type: 'doc',
            id: 'ampersand/toc'
        },

    ],
    ampersandGuideElems: [
        'ampersand/configuring-your-application',
        'ampersand/the-excel-importer',
        'ampersand/the-command-line-tool',
    ],
    ampersandReferenceElems: [

    ],
    ampersandMainSidebar: [

        {
            type: 'doc',
            id: 'ampersand/intro',
        }
        ,

        {
            label: 'Theory & background',
            type: 'category',
            items: [
                'ampersand/why-ampersand/whyAmpersand',
                'ampersand/why-ampersand/BRManifestoAndAmpersand',
                'ampersand/reactive-programming',
                'ampersand/research',
                'ampersand/future-plans',
            ]
        },
    ].concat((
        [

            {
                label: 'Reference',
                type: 'category',
                items: [
                    'ampersand/the-language-ampersand/README',
                    'ampersand/the-language-ampersand/atoms',
                    'ampersand/the-language-ampersand/best-practices',
                    'ampersand/the-language-ampersand/context',
                    'ampersand/the-language-ampersand/current-date',
                    'ampersand/the-language-ampersand/design-considerations',
                    'ampersand/the-language-ampersand/how-to-read-syntax-statements',
                    'ampersand/the-language-ampersand/language-support',
                    'ampersand/the-language-ampersand/meaning-statements',
                    'ampersand/the-language-ampersand/patterns',
                    'ampersand/the-language-ampersand/relations',
                    'ampersand/the-language-ampersand/services/README',
                    'ampersand/the-language-ampersand/services/crud',
                    'ampersand/the-language-ampersand/services/example-client',
                    'ampersand/the-language-ampersand/services/example-login',
                    'ampersand/the-language-ampersand/services/explanations',
                    'ampersand/the-language-ampersand/services/layout-of-user-interfaces/README',
                    'ampersand/the-language-ampersand/services/layout-of-user-interfaces/your-own-widgets-html-and-css',
                    'ampersand/the-language-ampersand/services/syntax-of-interface-statements',
                    'ampersand/the-language-ampersand/syntactical-conventions/README',
                    'ampersand/the-language-ampersand/syntactical-conventions/explanations',
                    'ampersand/the-language-ampersand/syntactical-conventions/language-support',
                    'ampersand/the-language-ampersand/syntactical-conventions/patterns',
                    'ampersand/the-language-ampersand/syntactical-conventions/the-concept-statement',
                    'ampersand/the-language-ampersand/syntactical-conventions/the-context-statement',
                    'ampersand/the-language-ampersand/syntactical-conventions/the-ident-statement',
                    'ampersand/the-language-ampersand/syntactical-conventions/the-include-statement',
                    'ampersand/the-language-ampersand/syntactical-conventions/the-population-statement',
                    'ampersand/the-language-ampersand/syntactical-conventions/the-purpose-statement',
                    'ampersand/the-language-ampersand/syntactical-conventions/the-relation-statement',
                    'ampersand/the-language-ampersand/syntactical-conventions/the-rule-statement',
                    'ampersand/the-language-ampersand/syntactical-conventions/the-table-statement',
                    'ampersand/the-language-ampersand/terms/README',
                    'ampersand/the-language-ampersand/terms/semantics',
                    'ampersand/the-language-ampersand/the-classify-statement',
                    'ampersand/the-language-ampersand/the-concept-statement',
                    'ampersand/the-language-ampersand/the-enforce-statement',
                    'ampersand/the-language-ampersand/the-ident-statement',
                    'ampersand/the-language-ampersand/the-include-statement',
                    'ampersand/the-language-ampersand/the-population-statement',
                    'ampersand/the-language-ampersand/the-preprocessor',
                    'ampersand/the-language-ampersand/the-purpose-statement',
                    'ampersand/the-language-ampersand/the-rule-statement',
                    'ampersand/the-language-ampersand/the-table-statement',
                    'ampersand/the-language-ampersand/truth',

                ]
            },

            {
                label: 'More documents',
                type: 'category',
                items: [
                    'ampersand/conceptual/why-declarative',
                    'ampersand/architecture-of-an-ampersand-application/README',
                    'ampersand/architecture-of-an-ampersand-application/backend-framework',
                    'ampersand/architecture-of-an-ampersand-application/extensions/README',
                    'ampersand/architecture-of-an-ampersand-application/extensions/the-execengine',
                    'ampersand/architecture-of-an-ampersand-application/hooks',
                    'ampersand/docker/README',
                    'ampersand/docker/compiler',
                    'ampersand/docker/modelling-environment',
                    'ampersand/docker/prototype-database',
                    'ampersand/docker/prototype-multi-stage-build',
                    'ampersand/exercises/README',
                    'ampersand/exercises/delivery',
                    'ampersand/exercises/vog-in-dutch',
                    'ampersand/governance/README',
                    'ampersand/installing-ampersand/README',
                    'ampersand/installing-ampersand/deploying-your-prototype',
                    'ampersand/installing-ampersand/installing-the-tools-manually',
                    'ampersand/modeling/README',
                    'ampersand/modeling/architecture',
                    'ampersand/modeling/conceptual-modeling',
                    'ampersand/modeling/data-modeling',
                    'ampersand/modeling/legal-modeling',
                    'ampersand/modeling/limitations-of-ampersand',
                    'ampersand/modeling/metamodeling',
                    'ampersand/modeling/properties',
                    'ampersand/ownership/README',

                    'ampersand/reusing-available-modules',
                    'ampersand/troubleshooting',
                    'ampersand/tutorial-rap3/README',
                    'ampersand/tutorial-rap3/conceptual-model-enrollment',
                    'ampersand/tutorial-rap3/example-system-enrollment',
                    'ampersand/tutorial-rap3/making-your-first-ampersand-script',
                    'ampersand/tutorial-rap3/your-tool-rap3',
                    'ampersand/why-ampersand/BRManifestoAndAmpersand',
                    'ampersand/why-ampersand/whyAmpersand'
                ]
            },
        ]
    )
    ),



    prototypeGuideElems: [
        'prototype/README',
        'prototype/config',
        'prototype/file-system',
        'prototype/event-dispatcher',
        'prototype/interface-templates',
        'prototype/generics',
        'prototype/ampersand-compiler',
    ],
    ampersandReferenceElems: [
        'ampersand/empty'

    ],
    prototypeMainSidebar: [
        {
            label: 'Prototype stuff',
            type: 'category',
            items: [
                'ampersand/empty'
            ]
        },

    ],
    toolsWeUseMainSidebar: [
        {
            label: 'Tools for contributors',
            type: 'category',
            items: [
                'the-tools-we-use/README',
                'the-tools-we-use/SUMMARY',
                'the-tools-we-use/ampersand-language-support',
                'the-tools-we-use/authentication-and-access-management-with-oauth',
                'the-tools-we-use/automation-of-releasing-ci-cd/README',
                'the-tools-we-use/automation-of-releasing-ci-cd/github-packages',
                'the-tools-we-use/building/README',
                'the-tools-we-use/building/automated-builds',
                'the-tools-we-use/building/building-an-ampersand-compiler-as-docker-image',
                'the-tools-we-use/building/haskell',
                'the-tools-we-use/building/testing-with-docker-on-your-own-laptop',
                'the-tools-we-use/deploying-rap3-with-azure',
                'the-tools-we-use/deploying-rap3-with-azure/deploying-rap3-with-azure-on-windows-server',
                'the-tools-we-use/deploying-with-kubernetes',
                'the-tools-we-use/functionality-of-rap3/README',
                'the-tools-we-use/functionality-of-rap3/account-manager',
                'the-tools-we-use/functionality-of-rap3/graduate-student',
                'the-tools-we-use/functionality-of-rap3/logging-in',
                'the-tools-we-use/functionality-of-rap3/student',
                'the-tools-we-use/functionality-of-rap3/tutor',
                'the-tools-we-use/git',
                'the-tools-we-use/gitbook/README',
                'the-tools-we-use/gitbook/dos-and-donts-in-ampersand-documentation',
                'the-tools-we-use/gitbook/getting-started-with-gitbook',
                'the-tools-we-use/group-1/development-using-vs-code',
                'the-tools-we-use/installation-of-rap/README',
                'the-tools-we-use/installation-of-rap/deploying-ounl-rap3',
                'the-tools-we-use/installation-of-rap/deploying-rap3-with-azure-on-ubuntu',
                'the-tools-we-use/installation-of-rap/deploying-to-your-own-laptop',
                'the-tools-we-use/installation-of-rap/deployment-configuration',
                'the-tools-we-use/installation-of-rap/details',
                'the-tools-we-use/installation-of-rap/making-docker-images',
                'the-tools-we-use/installation-of-rap/redeploying-rap3',
                'the-tools-we-use/klad',
                'the-tools-we-use/making-images',
                'the-tools-we-use/prototype-framework',
                'the-tools-we-use/rap3-student',
                'the-tools-we-use/rap3-tutor',
                'the-tools-we-use/releasing-ampersand-and-workflow-details',
                'the-tools-we-use/tools-used-in-the-ampersand-project'
            ]
        }

    ],
    notOnOldSite: [
        'ampersand/conceptual/theory',
        'ampersand/conceptual/automated-rules',

    ]
};