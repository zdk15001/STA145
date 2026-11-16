# Turning on usernames and the class leaderboard

The leaderboard is a Google Sheet that you own, plus the script in `Code.gs`. Students never see the sheet. The game talks to the script, and the script reads and writes the sheet.

## How approval works

A new username lands in the sheet with an empty checkbox in column C. Until you tick that box:

- the username is not on the leaderboard and not on the login page;
- the student cannot log in with it, even with the right code;
- the student can keep playing on the device where they created it, and their progress is saved.

Tick the box to approve. Delete the row to refuse: the student is told their username was not approved and carries on anonymously with their progress. If a student changes their username, the box un-ticks itself and you approve the new name. Ticking boxes works in the Google Sheets phone app too.

**Do not approve a username that identifies a student** (a name, initials with a class year, a student ID). Approval is the step that keeps the leaderboard anonymous to classmates.

## Before you turn it on

With the leaderboard off, the game sends nothing anywhere, so there is nothing to clear with anyone. Turning it on means a script under your Google account receives usernames and game progress from students. Four things are worth settling first:

1. **Vetting.** TCNJ's Information Security guidance says to use only software the security team has vetted, and to ask your Support Specialist about anything that has not been. Ask them about a Google Apps Script web app attached to a Sheet in your TCNJ Google Drive.
2. **Open access.** The script has to be deployed so that "Anyone" can reach it, because students are not signed in to Google when they play. The class code is what keeps strangers out. Ask whether that is acceptable for a Sheet in your TCNJ account. If the "Anyone" choice is missing when you deploy, TCNJ has switched it off: stop there and ask IT rather than moving the Sheet to a personal account.
3. **Student records.** The game is tied to an assignment, so decide how credit is recorded before you start. The cleanest way is inside Canvas (for example, a screenshot of the student's room), which works for anonymous players too and keeps the link between a student and a score in Canvas. Do not keep a list of who has which username: with no such list, the sheet holds nothing that identifies a student. Ask Records and Registration whether a public, opt-in leaderboard of made-up usernames needs anything more.
4. **Short codes.** A 2-digit login code can be guessed. Anyone with the class code who knows a classmate's username gets three tries an hour. If that worries you, set `CODE_DIGITS` to 4 near the top of `Code.gs` before you deploy.

## What the sheet will hold

One row per player: a random row ID, the username, the approved checkbox, level, number of perfect scores, best score per level, stars, failed tries per level, decoration choices, three timestamps, and a count of wrong login codes tried against that username.

It never holds real names, email addresses, student IDs, internet addresses, passwords or login codes. A login code is worked out fresh each time it is checked, from the row ID and a private key that only the script can read.

## Set up (about ten minutes)

1. In your TCNJ Google Drive, make a new Google Sheet. Name it something like *Room for Error leaderboard*. Do not share it with anyone.
2. In the Sheet, choose **Extensions → Apps Script**.
3. Delete whatever is in the editor, paste in all of `Code.gs`, and save.
4. Go back to the Sheet and reload the page. A **Room for Error** menu appears next to Help.
5. Choose **Room for Error → 1. Set up this sheet**. Google asks you to authorize the script the first time. It asks to see and edit this one spreadsheet and to show menus and message boxes in it, and nothing else.
6. A box shows your **class code**. Note it. Each new username will arrive in the `players` tab with its own checkbox in column C.
7. Back in the Apps Script editor, choose **Deploy → New deployment**. Click the gear and pick **Web app**. Set **Execute as** to *Me* and **Who has access** to *Anyone*. Click **Deploy**.
8. Copy the **Web app URL**. It ends in `/exec`.
9. In `config.js`, paste that address between the quotes after `leaderboardUrl:` and commit the change.
10. Post this link in Canvas, with your own username, repository and class code filled in: `https://YOUR-USERNAME.github.io/REPOSITORY/room-for-error/?class=CLASSCODE`. The class code is filled in for students who use the link. Post `PRIVACY.md` with it.

Test it yourself before posting: open the link, create a made-up username, check that a row appears in the sheet, tick its box, and log in from another device.

## During the semester

- **New usernames.** Open the sheet and tick the box in column C for each one you approve. **Room for Error → Show usernames waiting for approval** lists them. Students cannot log in from another device until you do this, so check often in the first week.
- **A username is inappropriate or identifies a student.** Before approval, delete the row. After approval, un-tick the box or type a new username over it in column B.
- **A student lost their login code.** Choose **Room for Error → Give a player a new login code**, type their username, and give them the new code. The student has to tell you their username for this, so do it in person or in a Canvas message, and do not keep a list.
- **Someone is guessing codes.** The last column counts wrong codes tried against each username. A large number there means someone has been guessing. Give that player a new code, and consider a new class code.
- **A student wants to be removed.** They can do it themselves from their account. You can also delete their row.
- **The class code got out.** Choose **Room for Error → Make a new class code** and post the new link in Canvas. Students who already have a username are asked for it once.

## After the semester

Choose **Room for Error → Delete all players (end of semester)**, then make a new class code before the next class uses the sheet. Students keep whatever is saved in their own browsers.

## If you change the script later

Paste the new `Code.gs`, save, then choose **Deploy → Manage deployments**, click the pencil, set **Version** to *New version*, and click **Deploy**. The web app address stays the same.

## Turning the leaderboard off again

Empty the quotes after `leaderboardUrl:` in `config.js` and commit. To shut the script down as well, choose **Deploy → Manage deployments** and archive the deployment.
