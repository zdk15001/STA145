# Room for Error

**Created with the help of Claude AI. Many Errors are Present.**

A 20-level practice game for STA 145: Applied Statistics for Social Scientists. Students answer five questions per level from the course workbook. Passing a level (4 of 5) unlocks a statistics-themed decoration for a bedroom they furnish as they go. A missed level gets easier on the next try.

Created with the help of [Claude AI](https://claude.com/), an AI model made by Anthropic, for Professor Kline's STA 145. The questions were written with AI from the workbook's Terms and Concepts and have not all been reviewed by a person. The same notice sits at the top of every screen of the game.

This is a course practice tool. It is not an official website of The College of New Jersey.

## What students see

The first screen offers three choices.

- **Play anonymously.** No username. Progress is saved in the student's own browser and nothing is sent anywhere. A backup code in their account box moves progress to another device.
- **Create a username.** The student picks a username and is given a 2-digit login code to write down. The username is not shown to anyone, and cannot be used to log in, until you approve it in your Google Sheet. Students are told that usernames are public, that they should not include anything identifiable, and that they can get full credit without creating an account.
- **Log in.** The student picks their username from a list of approved usernames and types their code.

With the leaderboard off (the default), only the first choice exists. The game never asks for a real name, email address, student ID or a password.

## What is in this folder

| File or folder | What it is |
| --- | --- |
| `index.html` | The page. |
| `config.js` | The only file you need to edit: leaderboard address and the wording of the About box. |
| `js/q_core.js`, `js/q_m1a.js` … `js/q_m3.js` | The question bank: 228 question templates, grouped by workbook section. |
| `js/r_outputs.js` | R output used by the t-test, ANOVA and regression questions. |
| `js/room.js` | The bedroom and its 60 decorations. |
| `js/app.js` | The game itself: levels, saving, account box, leaderboard screen. |
| `js/net.js` | The leaderboard connection and the backup codes. |
| `css/style.css`, `fonts/` | Styles and the three fonts (stored here, so no font service is contacted). |
| `leaderboard/Code.gs` | The script for the optional Google Sheet leaderboard. |
| `leaderboard/SETUP.md` | How to turn the leaderboard on, and how to run it during the semester. |
| `PRIVACY.md` | The student notice, in a form you can paste into Canvas. |

There is no build step. Edit a file, commit, and the page changes.

## Put it on GitHub Pages

1. Add this `room-for-error` folder to the repository and commit.
2. On GitHub, open the repository, then **Settings → Pages**.
3. Under **Build and deployment**, set **Source** to *Deploy from a branch*, choose the branch (`main`) and the folder `/ (root)`, and save.
4. After a minute or two the game is at `https://YOUR-USERNAME.github.io/REPOSITORY/room-for-error/`.

Turning Pages on for a repository publishes every file in that branch as a web page. If you would rather publish only the game, put this folder in a repository of its own.

In this state the game collects nothing. Every student plays anonymously, each student's progress lives in their own browser, and the backup code is how they move it between devices.

## Turn on usernames and the class leaderboard (optional)

Follow `leaderboard/SETUP.md`. It takes about ten minutes and uses a Google Sheet that you own. Read the "Before you turn it on" section there first: it lists the questions to settle with TCNJ offices.

## What the page is allowed to do

`index.html` carries a content-security rule that the browser enforces:

- it loads files only from this folder;
- it can send data to one place only, a Google Apps Script web app (`script.google.com`), and only if `config.js` names one;
- it sets no cookies and loads no analytics, ads, fonts or scripts from anywhere else.

`js/net.js` also refuses any leaderboard address that is not a Google Apps Script web app address.

## Changing a question

Open the file for the workbook module (`js/q_m1a.js` covers Levels 1 to 5, `q_m1b.js` Levels 6 to 10, `q_m2.js` Levels 11 to 14, `q_m3.js` Levels 15 to 20). Each question is one function inside a `level({...}, [ ... ])` block. Edit the wording or delete the function, and commit.

## Accessibility

Built to WCAG 2.1 Level AA: keyboard operable, labelled for screen readers, readable at 400% zoom and in dark mode, with a text description for every chart. Automated checks and keyboard testing were run on every screen. No tool can certify conformance, and it has not yet been tried by a student who uses a screen reader, so the About box tells students how to ask for the same practice in another format.

## Two limits to know about

- **Scores are not secure.** Everything runs in the student's browser, so a determined student can give themselves any score. Do not treat a leaderboard position as proof of work.
- **Login codes are short.** A 2-digit code is easy to remember and easy to guess: one chance in 100 per try. The script allows three wrong tries an hour per username and counts wrong tries in the sheet. To make codes longer, change `CODE_DIGITS` at the top of `leaderboard/Code.gs` (4 gives one chance in 10,000) and deploy a new version.

## Credits and licences

- Game, questions and artwork: created with the help of Claude AI (Anthropic) for STA 145.
- Fonts: Atkinson Hyperlegible, Bricolage Grotesque and JetBrains Mono, each under the SIL Open Font License 1.1. See `fonts/LICENSES.txt`.
