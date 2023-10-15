# Onboarding

> Welcome to the Ampersand team!

Below you wil find a list of activities to get a new contributor up-to speed with Ampersand development.

- Schedule a meeting with [Stef Joosten](<mailto:Stef.Joosten@ordina.nl?subject=Request demo meeting>) to get an explanation and demo about the Ampersand project.
- Contact [Han Joosten](<mailto:Han.Joosten@ordina.nl?subject=Request for access to ampersand project&body=Hi Han,%0D%0A%0D%0AI am contacting you to get access to the following:%0D%0A  - An invite to the upcoming sprint review meeting to meet the team.%0D%0A  - Access to the github repository, _YOUR_GITHUB_ACCOUNTNAME_OR_ACCOUNTLINK_.%0D%0A  - Invites to the different sprint meetings that are currently scheduled.%0D%0A  - Adding me to the Ampersand-whatsapp group, my phone number is: YOUR_PHONE_NUMBER.>) for the following options to get access to the full ampersand project.
  - Join the upcoming sprint review meeting to meet the team.
  - Request access to the GitHub repository as [described below](#github-memberships).
  - Get invited to the different sprint meetings through teams as [described below](#microsoft-teams-memberships).
- Clone the needed repositories [described below](#github-workflow-and-info)

## GitHub memberships

Ensure you have a GitHub account. This can be a 'professional' account connected to your Ordina email or any other account.

Request membership to

- [AmpersandTarski](https://github.com/orgs/AmpersandTarski/people) organization and
- [Ordina A-team](https://github.com/orgs/AmpersandTarski/teams/ordina-a-team/members).

<Table>
  <tr>
    <td>⚠️<b>WARNING</b>⚠️</td>
  </tr>
  <tr>
    <td>Make sure when you contact Han about this that you provide him with your Github account name (or URL to your GitHub profile page).
    </td>
  </tr>
</Table>

## Microsoft Teams memberships

The Ampersand team members and also some old members can be found in the teams group channel.

- Membership Microsoft Teams project [Project A & NUT-Vluchtelingen open source
  ](https://teams.microsoft.com/l/team/19%3ayM9P1tFiWIADqDUbLDyX7ksB1Oavi04StkxyS6grh7A1%40thread.tacv2/conversations?groupId=09b86f1c-3ba6-411d-9b16-f0915eb2ed8a&tenantId=a254b169-0a6b-47f9-af4c-169704421c2e).
- Owner permissions on that project (seems to be current practice).
- Tagged as _AmpersandTarski A-team_ (facilitates initiating chats).

## Meetings

Request invitations to the regular (Microsoft Teams) meetings.

These are currently:

- NUTwente/Ampersand wekelijkse overleg; every monday 11:30, 1 hour.
- Daily standup; tuesday till friday 11:30, 30 minutes
- A-team: Sprint review; at end of sprint, friday 9:00, 50 minutes
- A-team: Sprint retrospective; at end of sprint, friday 10:00, 50 minutes
- A-team: Sprint planning; at end of sprint, friday 11:00, 50 minutes

## Github workflow and info

To work on the project you wil need a local version of the different repositories depending on what you are going to work on. Below is a short description of all the different repositories and what they are used for.

### Ampersand

[The ampersand repository](https://github.com/AmpersandTarski/Ampersand) contains the ampersand application that compiles and builds ampersand applications.

### Prototype

[The prototype repository](https://github.com/AmpersandTarski/prototype) makes sure that a ampersand application has a visual interface for the user to interact with your applications.

### RAP

[The RAP repository](https://github.com/AmpersandTarski/RAP) is an ampersand application that can be used to create a new ampersand application. This app is mainly used by the Open University of the Netherlands in the course Rule Based Design.

### AmpersandTarski.github.io

[The AmpersandTarski.github.io repository](https://github.com/AmpersandTarski/AmpersandTarski.github.io) is used to create the docusaurus documentation webpage. It pulls the different documents from the docs folders of all the repositories. While working this way we need to make sure to work in the correct docs folders.

> For Example a document that references or is meant for RAP needs to be in the RAP repository docs folder.

<Table>
  <tr>
    <td>⚠️<b>WARNING</b>⚠️</td>
  </tr>
  <tr>
    <td>Make sure to work in the <b><i>documentation branch</i></b> while creating or editing documentation to prevent unnecessary usage of the pipelines. 
    </td>
  </tr>
</Table>
