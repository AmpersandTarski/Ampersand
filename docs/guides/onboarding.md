# Onboarding

>Welcome to the Ampersant team!

Below you wil find a list of activities to get a new contributor up-to speed with Ampersant development.

 - Schedule a meeting with [Stef Joosten](<mailto:Stef.Joosten@ordina.nl?subject=Request demo meeting&body=Hi Stef,>) to get an explanation and demo about the Ampersant project.
 - Join the upcomming sprint review meeting to meet the team.
 - Request access to the GitHub repository as [described below](#github-memberships). 
 - Get invited to the different sprint meetings through teams as [described below](#microsoft-teams-memberships).
 - Clone the needed repositories [described below](#github-workflow-and-info)

## GitHub memberships

Ensure you have a GitHub account. This can be a 'professional' account connected to your Ordina email or any other account.

Request membership to

- [AmpersandTarski](https://github.com/orgs/AmpersandTarski/people) organization and
- [Ordina A-team](https://github.com/orgs/AmpersandTarski/teams/ordina-a-team/members).

For this, contact [Han Joosten](<mailto:Han.Joosten@ordina.nl?subject=Request for github access>), and provide him with your Github account name (or URL to your GitHub profile page).

## Microsoft Teams memberships

Request

- Membership Microsoft Teams project [Project A & NUT-Vluchtelingen opensource
  ](https://teams.microsoft.com/l/team/19%3ayM9P1tFiWIADqDUbLDyX7ksB1Oavi04StkxyS6grh7A1%40thread.tacv2/conversations?groupId=09b86f1c-3ba6-411d-9b16-f0915eb2ed8a&tenantId=a254b169-0a6b-47f9-af4c-169704421c2e).
- Owner permissions on that project (seems to be current practise).
- Tagged as _AmpersandTarski A-team_ (facilitates initiating chats).

For these authorizations, contact [Fran Slot](<mailto:fran.slot@ordina.nl?subject=Request for A-team authorization>).

## Meetings

Request invitations to the regular (Microsoft Teams) meetings.

These are currently:

- NUTwente/Ampersand wekelijkse overleg; every monday 11:30, 1 hour.
- Daily standup; tuesday till friday 11:30, 30 minutes
- A-team: Sprint review; at end of sprint, friday 9:00, 50 minutes
- A-team: Sprint retrospective; at end of sprint, friday 10:00, 50 minutes
- A-team: Sprint planning; at end of sprint, friday 11:00, 50 minutes

For these invites, contact [Fran Slot](<mailto:fran.slot@ordina.nl?subject=Request for invitations of A-team meetings>).

## Github workflow and info

To work on the project you wil need a local version of the different repositories depending on what you are going to work on. Below is a short description of all the different repositories and what they are used for.

### Ampersant

[The ampersant repository](https://github.com/AmpersandTarski/Ampersand) contains the ampersant application that compiles and builds ampersant applications. 

### Prototype

[The prototype repository](https://github.com/AmpersandTarski/prototype) makes sure that a ampersant application has a visual interface for the user to interact with your applications.

### RAP

[The RAP repository](https://github.com/AmpersandTarski/RAP) is an ampersant application that can be used to create a new ampersant application. This app is mainly used by the Open University of the Netherlands in the course Rule Based Design.

### AmpersandTarski.github.io

[The AmpersandTarski.github.io repository](https://github.com/AmpersandTarski/AmpersandTarski.github.io) is used to create the docusaurus documentation webpage. It pulls the different documents from the docs folders of all the repositories. While working this way we need to make sure to work in the correct docs folders.
>For Example a document that references or is meant for RAP needs to be in the RAP repository docs folder.


|| ⚠️**WARNING** ||
| Make sure to work in the documenation branch while creating or editing documentation to prevent unnecessary usage of the pipelines. |
