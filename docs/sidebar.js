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
                    'ampersand/Conceptual/why-declarative',
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
        'Prototype/README',
    ],
    prototypeMainSidebar: [
        {
            label: 'Prototype stuff',
            type: 'category',
            items: [
                'Prototype/ampersand-compiler',
                'Prototype/config',
                'Prototype/event-dispatcher',
                'Prototype/file-system',
                'Prototype/generics',
                'Prototype/interface-templates'
            ]
        },

    ],
    toolsWeUseMainSidebar: [
        {
            label: 'Tools for contributors',
            type: 'category',
            items: [
                'TheToolsWeUse/README',
                'TheToolsWeUse/SUMMARY',
                'TheToolsWeUse/ampersand-language-support',
                'TheToolsWeUse/authentication-and-access-management-with-oauth',
                'TheToolsWeUse/automation-of-releasing-ci-cd/README',
                'TheToolsWeUse/automation-of-releasing-ci-cd/github-packages',
                'TheToolsWeUse/building/README',
                'TheToolsWeUse/building/automated-builds',
                'TheToolsWeUse/building/building-an-ampersand-compiler-as-docker-image',
                'TheToolsWeUse/building/haskell',
                'TheToolsWeUse/building/testing-with-docker-on-your-own-laptop',
                'TheToolsWeUse/deploying-rap3-with-azure',
                'TheToolsWeUse/deploying-rap3-with-azure/deploying-rap3-with-azure-on-windows-server',
                'TheToolsWeUse/deploying-with-kubernetes',
                'TheToolsWeUse/functionality-of-rap3/README',
                'TheToolsWeUse/functionality-of-rap3/account-manager',
                'TheToolsWeUse/functionality-of-rap3/graduate-student',
                'TheToolsWeUse/functionality-of-rap3/logging-in',
                'TheToolsWeUse/functionality-of-rap3/student',
                'TheToolsWeUse/functionality-of-rap3/tutor',
                'TheToolsWeUse/git',
                'TheToolsWeUse/gitbook/README',
                'TheToolsWeUse/gitbook/dos-and-donts-in-ampersand-documentation',
                'TheToolsWeUse/gitbook/getting-started-with-gitbook',
                'TheToolsWeUse/group-1/development-using-vs-code',
                'TheToolsWeUse/installation-of-rap/README',
                'TheToolsWeUse/installation-of-rap/deploying-ounl-rap3',
                'TheToolsWeUse/installation-of-rap/deploying-rap3-with-azure-on-ubuntu',
                'TheToolsWeUse/installation-of-rap/deploying-to-your-own-laptop',
                'TheToolsWeUse/installation-of-rap/deployment-configuration',
                'TheToolsWeUse/installation-of-rap/details',
                'TheToolsWeUse/installation-of-rap/making-docker-images',
                'TheToolsWeUse/installation-of-rap/redeploying-rap3',
                'TheToolsWeUse/klad',
                'TheToolsWeUse/making-images',
                'TheToolsWeUse/prototype-framework',
                'TheToolsWeUse/rap3-student',
                'TheToolsWeUse/rap3-tutor',
                'TheToolsWeUse/releasing-ampersand-and-workflow-details',
                'TheToolsWeUse/tools-used-in-the-ampersand-project'
            ]
        }

    ],
    notOnOldSite: [
        'ampersand/Conceptual/theory',
        'ampersand/Conceptual/Automated Rules',

    ]
};