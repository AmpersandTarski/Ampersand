module.exports = {
    ampersandLandingpagesSidebar: [
        {
            label: 'Interested visitor',
            type: 'doc',
            id: 'ampersand/landingpage/interested-visitor'
        },
        {
            label: 'Student',
            type: 'doc',
            id: 'ampersand/landingpage/student'
        },
        {
            label: 'Ampersand User',
            type: 'doc',
            id: 'ampersand/landingpage/ampersand-user'
        },
        {
            label: 'Scientist',
            type: 'doc',
            id: 'ampersand/landingpage/scientist'
        },
        {
            label: 'Contributor',
            type: 'doc',
            id: 'ampersand/landingpage/contributor'
        }
    ],
    ampersandReferenceSidebar: [
        // This is for all documentation from the Ampersand repo that should go in the `Reference material` part of the menu. 
        {
            label: 'Dictionary',
            type: 'doc',
            id: 'ampersand/reference-material/dictionary'
        },
        {
            label: 'Video tutorials',
            type: 'doc',
            id: 'ampersand/video-tutorials'
        },
        {
            label: 'Meta syntax',
            type: 'doc',
            id: 'ampersand/reference-material/how-to-read-syntax-statements'
        },
        {
            label: 'Syntax of Ampersand',
            type: 'doc',
            id: 'ampersand/reference-material/syntax-of-ampersand'
        },
        {
            label: 'Atoms',
            type: 'doc',
            id: 'ampersand/reference-material/atoms'
        },
        {
            label: 'Contexts',
            type: 'doc',
            id: 'ampersand/reference-material/context'
        },
        {
            label: 'Terms',
            type: 'doc',
            id: 'ampersand/reference-material/terms'
        },
        {
            label: 'Interfaces',
            type: 'doc',
            id: 'ampersand/reference-material/interfaces'
        },
        {
            label: 'Architecture of generated systems',
            type: 'doc',
            id: 'ampersand/reference-material/architecture-of-an-ampersand-application'
        },
        {
            label: 'Preprocessor',
            type: 'doc',
            id: 'ampersand/reference-material/the-preprocessor'
        },
        {
            label: 'Importing data from Excel',
            type: 'doc',
            id: 'ampersand/the-excel-importer'
        },
        {
            label: 'Command line functions',
            type: 'doc',
            id: 'ampersand/the-command-line-tool'
        },
    ],
    ampersandGuideSidebar: [
        // This is for all documentation from the Ampersand repo that should go in the `Guides` part of the menu. 
        {
            label: 'Frequently asked questions',
            type: 'doc',
            id: 'ampersand/guides/frequently-asked-questions'
        },
        {
            label: 'Installing Ampersand',
            type: 'doc',
            id: 'ampersand/guides/installing-ampersand'
        },
        {
            label: 'Deploying your prototype',
            type: 'doc',
            id: 'ampersand/guides/deploying-your-prototype'
        },
        {
            label: 'Manual installation',
            type: 'doc',
            id: 'ampersand/guides/best-practices'
        },
        {
            label: 'Contributor\'s guide',
            type: 'category',
            items: [
                'ampersand/guides/onboarding',
                'ampersand/the-tools-we-use/README',
                'ampersand/the-tools-we-use/ampersand-language-support',
                'ampersand/the-tools-we-use/authentication-and-access-management-with-oauth',
                'ampersand/the-tools-we-use/automation-of-releasing-ci-cd/README',
                'ampersand/the-tools-we-use/automation-of-releasing-ci-cd/github-packages',
                'ampersand/the-tools-we-use/building/README',
                'ampersand/the-tools-we-use/building/automated-builds',
                'ampersand/the-tools-we-use/building/building-an-ampersand-compiler-as-docker-image',
                'ampersand/the-tools-we-use/building/haskell',
                'ampersand/the-tools-we-use/building/testing-with-docker-on-your-own-laptop',
                'ampersand/the-tools-we-use/testing-infrastructure',
                'ampersand/the-tools-we-use/deploying-rap3-with-azure',
                'ampersand/the-tools-we-use/deploying-rap3-with-azure/deploying-rap3-with-azure-on-windows-server',
                'ampersand/the-tools-we-use/deploying-with-kubernetes',
                'ampersand/the-tools-we-use/functionality-of-rap3/README',
                'ampersand/the-tools-we-use/git',
                'ampersand/the-tools-we-use/group-1/development-using-vs-code',
                'ampersand/the-tools-we-use/klad',
                'ampersand/the-tools-we-use/making-docker-images',
                'ampersand/the-tools-we-use/prototype-framework',
                'ampersand/the-tools-we-use/rap3-student',
                'ampersand/the-tools-we-use/rap3-tutor',
                'ampersand/the-tools-we-use/releasing-ampersand-and-workflow-details',
                'ampersand/the-tools-we-use/tools-used-in-the-ampersand-project'
            ]
        },
        {
            label: 'Best practices for Ampersand modellers',
            type: 'doc',
            id: 'ampersand/guides/best-practices'
        },
        {
            label: 'Configuring your application',
            type: 'doc',
            id: 'ampersand/reference-material/configuring-your-application'
        },
        {
            label: 'A list of all instructional videos',
            type: 'doc',
            id: 'ampersand/videos'
        },
    ],
    ampersandTheorySidebar: [
        {
            label: 'Design considerations',
            type: 'doc',
            id: 'ampersand/reference-material/design-considerations'
        },
        {
            type: 'doc',
            id: 'ampersand/reference-material/truth'
        },
        {
            type: 'doc',
            id: 'ampersand/conceptual/theory'
        },
        {
            type: 'doc',
            id: 'ampersand/reactive-programming'
        },
        {
            type: 'doc',
            id: 'ampersand/research'
        },
        {
            type: 'doc',
            id: 'ampersand/future-plans'
        },

    ],
    ampersandMainSidebar: [

        {
            type: 'doc',
            id: 'ampersand/intro',
        }
        ,

    ].concat((
        [

            {
                label: 'More documents',
                type: 'category',
                items: [
                    'ampersand/conceptual/why-declarative',
                    'ampersand/docker/README',
                    'ampersand/docker/compiler',
                    'ampersand/docker/modelling-environment',
                    'ampersand/docker/prototype-database',
                    'ampersand/docker/prototype-multi-stage-build',
                    'ampersand/exercises',
                    'ampersand/modeling/README',
                    'ampersand/modeling/architecture',
                    'ampersand/modeling/conceptual-modeling',
                    'ampersand/modeling/data-modeling',
                    'ampersand/modeling/legal-modeling',
                    'ampersand/modeling/limitations-of-ampersand',
                    'ampersand/ownership/README',
                    'ampersand/reusing-available-modules',
                    'ampersand/troubleshooting',
                    'ampersand/tutorial-rap4',
                ]
            },
        ]
    )
    ),

    toolsWeUseMainSidebar: [
    ],
    notOnOldSite: [
        'ampersand/conceptual/automated-rules',

    ]
};