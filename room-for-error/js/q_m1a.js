/* Levels 1-5: Modules 1.1 to 1.4 (history, data tables, sampling, variable types, R basics) */
(() => {
  const { rnd, pick, shuffle, sample, sum, mean, fmt, NAMES, mc, cat, num, level } = QZ;

  /* ============ LEVEL 1: Module 1.1 ============ */
  const FRAMES = ['Abstract liberalism', 'Naturalization', 'Cultural racism', 'Minimization of racism'];
  const FRAME_WHY = {
    'Abstract liberalism': 'Abstract liberalism uses ideas like equal opportunity and individual choice to explain a racial gap while ignoring structural or institutional factors.',
    'Naturalization': 'Naturalization explains a racial gap as a natural occurrence, such as people simply preferring to be around others like themselves.',
    'Cultural racism': 'Cultural racism explains the standing of a racial group by pointing to its supposed culture, values, or habits.',
    'Minimization of racism': 'Minimization of racism claims that discrimination is mostly a thing of the past, limited to a few isolated people, and no longer a real barrier.'
  };
  const frameScenes = [
    ['A dean is shown data that Black students are underrepresented in the campus honors program. The dean says: "The application is open to everyone. Students who want it badly enough will apply."', 0],
    ['A landlord association is shown data that neighborhoods in its city are highly segregated by race. A spokesperson says: "People just like living near others who are similar to them. That\'s human nature."', 1],
    ['A commentator is shown data on racial gaps in college graduation rates and says: "Some communities just don\'t value education the way others do."', 2],
    ['A hiring manager is shown data that Black applicants get fewer callbacks than white applicants with the same resume. The manager says: "That kind of discrimination ended decades ago. Maybe there are a few bad apples, but it doesn\'t hold anyone back today."', 3],
    ['A student notices that tables in the dining hall are mostly sorted by race and says: "That\'s just what happens. People gravitate toward their own group."', 1],
    ['A bank executive is shown data that Black-owned businesses are approved for loans less often. The executive says: "Anyone with a solid plan and a strong work ethic can get a loan. It comes down to individual choices."', 0],
    ['A talk-show guest is shown data on the racial wealth gap and says: "It comes down to spending habits and how those families raise their kids."', 2],
    ['A city official is shown data that Black drivers are stopped by police more often than white drivers. The official says: "Racism isn\'t really a factor in policing anymore. People bring it up far too much."', 3]
  ];
  const TERMS = ['Institutional racism', 'Structural racism', 'Racial prejudice', 'Racial discrimination'];
  const TERM_WHY = 'Institutional racism: policies and practices that produce racial inequalities within a single institution, such as education or banking. Structural racism: the cumulative, long-term effect of those inequalities across many interconnected institutions. Racial prejudice: beliefs about people of other racial or ethnic identities. Racial discrimination: allocating resources based on perceived racial or ethnic identity.';
  const termScenes = [
    ['A school district\'s discipline policy results in Black students being suspended at three times the rate of white students for similar behavior.', 0],
    ['Decades of housing segregation shape which schools children attend, which shapes the jobs they can get, which shapes the wealth they pass to their own children.', 1],
    ['A person believes that people of another race are less trustworthy but never acts on that belief.', 2],
    ['A landlord refuses to rent an apartment to a family because of the ethnicity he assumes they are.', 3],
    ['A hospital system\'s referral practices result in Black heart patients being sent for advanced treatment less often than white patients with the same condition.', 0],
    ['Gaps in school funding, hiring, lending, and policing reinforce one another across generations, producing unequal life chances.', 1]
  ];
  const assumptions = [
    ['Individual effort and personal responsibility are the primary drivers of success or failure.', 0],
    ['Equal treatment under the law will most likely produce equal outcomes if people make the "right" choices.', 0],
    ['People are naturally drawn to those with similar racial or cultural identities.', 1],
    ['The preference for living among similar people is stronger than the desire to improve one\'s life chances.', 1],
    ['There is a correct cultural behavior that leads to universally preferred outcomes.', 2],
    ['Cultural traits that determine behavior are inherent, stable, and passed down.', 2],
    ['Racism exists mainly as isolated incidents of discrimination by "racist" people.', 3],
    ['The vast majority of people and organizations are racially neutral in how they make decisions.', 3]
  ];
  const PEOPLE = ['Francis Galton', 'Karl Pearson', 'Ronald A. Fisher', 'Lancelot Hogben', 'Thomas H. Morgan', 'Hermann J. Muller'];
  const peopleFacts = [
    ['coined the term "eugenics" in 1883', 0, 'Francis Galton coined "eugenics" in 1883 and is known as the father of eugenics. He also popularized correlation and regression to the mean.'],
    ['noticed that very tall parents tend to have children closer to average height, the idea behind regression to the mean', 0, 'Francis Galton studied the heights of parents and their adult children, which led to the idea of regression to the mean.'],
    ['named the standard deviation and worked out the formula for the correlation coefficient r', 1, 'Karl Pearson, Galton\'s protege, named the standard deviation and developed the formula for the correlation coefficient r. He was open that statistics was, for him, a tool for eugenics.'],
    ['introduced the chi-squared test', 1, 'Karl Pearson introduced the chi-squared test, which you use in Module 3.1.'],
    ['developed the analysis of variance (ANOVA) and the habit of using .05 as a significance cutoff', 2, 'R. A. Fisher developed ANOVA, randomized experiments, and the logic of significance testing. He was a committed eugenicist throughout his life.'],
    ['introduced the word "variance" in a 1918 paper on inheritance', 2, 'R. A. Fisher introduced the word "variance" in 1918 while splitting the variation in a trait into pieces.'],
    ['argued that genes and environment interact, pointing to fruit flies whose eye facets depended on both genetic strain and temperature', 3, 'Lancelot Hogben was an outspoken critic of eugenics. His central argument was about interaction: the effect of genes depends on the environment.'],
    ['ran the "fly room" at Columbia, where holding the environment constant let differences between flies be traced to genes', 4, 'Thomas Hunt Morgan could hold the environment constant in the lab. He criticized eugenicists for making claims about people living in very different environments.'],
    ['gave the 1932 talk "The Dominance of Economics over Eugenics"', 5, 'Hermann J. Muller told the Third International Eugenics Congress there was no scientific basis for claiming that poorer classes were genetically inferior.']
  ];
  const statFactor = [
    { s: 'A superintendent is shown that Black students are suspended at three times the rate of white students and responds: "This isn\'t about race. Those schools just have stricter principals."', stat: 'The gap in suspension rates', fac: 'How strict the principals are', d: ['The superintendent\'s job title', 'The number of schools in the district'] },
    { s: 'A lender is shown that Latino applicants are denied mortgages twice as often as white applicants with similar credit scores and responds: "It has nothing to do with race. Those applicants pick riskier neighborhoods."', stat: 'The gap in mortgage denial rates', fac: 'The neighborhoods applicants choose', d: ['The applicants\' credit scores', 'The lender\'s annual profit'] },
    { s: 'A league official is shown that few head coaching jobs go to Black candidates and responds: "Race isn\'t the issue. It\'s about who has the right connections."', stat: 'The gap in who gets head coaching jobs', fac: 'Having the right connections', d: ['The number of teams in the league', 'The official\'s own career'] }
  ];
  const critiques = [
    ['A critic says: "Under this theory, any explanation that doesn\'t point to structural or institutional racism gets labeled colorblind racism, so no evidence could ever count against it."', 'Unfalsifiable', 'A theory is unfalsifiable when no possible evidence could show it to be wrong. Critics argue colorblind racism theory can absorb any competing explanation.'],
    ['A critic says: "The theory treats individual rights and equal treatment as part of the problem, which puts it in tension with a philosophy built on those ideas."', 'Anti-liberal', 'Liberalism is a philosophy that promotes individual rights, civil liberties, democracy, and free enterprise. Critics argue the theory is in foundational tension with it.'],
    ['A critic says: "The theory downplays class. Deindustrialization and gaps in education and jobs explain more than it admits."', 'Neglects socioeconomic inequality', 'Critics such as William Julius Wilson argue the theory prioritizes racial over socioeconomic inequalities.']
  ];

  level({ title: 'Reading Racial Statistics', mod: '1.1', skill: 'Spot the frames of colorblind racism and place the statisticians who built (and challenged) eugenics.' }, [
    () => { const [s, i] = pick(frameScenes); return cat(s + '\n\nWhich frame of colorblind racism does this explanation use?', FRAMES[i], FRAMES, FRAME_WHY[FRAMES[i]]); },
    () => { const [s, i] = pick(termScenes); return cat(s + '\n\nWhich term best describes this?', TERMS[i], TERMS, TERM_WHY); },
    () => { const [s, i] = pick(assumptions); return cat('Which frame of colorblind racism rests on this assumption?\n\n"' + s + '"', FRAMES[i], FRAMES, 'An assumption is something taken for granted without proof. ' + FRAME_WHY[FRAMES[i]]); },
    () => { const [f, i, why] = pick(peopleFacts); const others = sample(PEOPLE.filter((_, k) => k !== i), 3); return mc('Which figure from the history of statistics ' + f + '?', PEOPLE[i], others, why); },
    () => { const c = pick([3, 4, 5]); return mc('All but one of these figures used statistics to promote eugenics. Which one was a critic of the eugenics movement?', PEOPLE[c], [PEOPLE[0], PEOPLE[1], PEOPLE[2]], 'Galton, Pearson, and Fisher supported eugenics. Hogben, Morgan, and Muller challenged the movement, mainly because its data could not separate heredity from environment.'); },
    () => { const v = pick(statFactor); const askStat = Math.random() < 0.5; return mc(v.s + '\n\n' + (askStat ? 'In Bonilla-Silva\'s terms, what is the racial statistic being explained?' : 'What non-racial factor is doing the explaining?'), askStat ? v.stat : v.fac, [askStat ? v.fac : v.stat, ...v.d], 'Colorblind racism is an ideology that attributes racial inequalities to non-racial factors. The racial statistic is the gap being explained. The non-racial factor is what the speaker offers as its cause.'); },
    () => { const [s, r, why] = pick(critiques); return cat(s + '\n\nWhich critique of colorblind racism theory is this?', r, ['Unfalsifiable', 'Anti-liberal', 'Neglects socioeconomic inequality', 'Relies on small samples'], why); },
    () => mc('A classmate says: "Anyone who uses one of the colorblind frames must secretly be prejudiced." What would Bonilla-Silva say?', 'No. Frame users often sincerely support racial equality.', ['Yes. The frames exist to hide prejudice that people know they hold.', 'Yes. Prejudice is the definition of colorblind racism.', 'It depends on whether the speaker holds power in an institution.'], 'Prejudice means beliefs about people of other racial or ethnic identities. Colorblind racism is an ideology that explains racial inequalities with non-racial factors. Bonilla-Silva titled his book "Racism Without Racists" because people who use the frames are not necessarily overt racists and often believe racial equality is good.'),
    () => mc('A sociologist says a child born in a well-funded school district has better "life chances." In Max Weber\'s sense, what does that mean?', 'A higher probability of accessing valued goods and opportunities', ['A guarantee of earning more than their parents did at the same age', 'A stronger work ethic than children raised elsewhere', 'A personal choice to pursue more education'], 'Life chances is Weber\'s term for the probability that someone, given their position in society, can access valued goods and opportunities such as education, income, health, and housing. It is a probability, never a guarantee.')
  ]);

  /* ============ LEVEL 2: Module 1.2 (data tables) ============ */
  const COLS = {
    class_year: ['first-year', 'sophomore', 'junior', 'senior'],
    streaming: ['Netflix', 'Hulu', 'YouTube', 'Max', 'Disney+'],
    hours_sleep: [5, 6, 7, 8, 9],
    roommates: [0, 1, 2, 3],
    commute: ['walk', 'bike', 'car', 'bus'],
    coffees_per_week: [0, 2, 3, 5, 7, 10]
  };
  function makeTable(minRows = 4, maxRows = 7) {
    const k = rnd(minRows, maxRows), cols = sample(Object.keys(COLS), rnd(2, 4)), names = sample(NAMES, k);
    const rows = names.map((nm) => [nm, ...cols.map((c) => pick(COLS[c]))]);
    return { head: ['name', ...cols], rows, cols, names, k };
  }
  const tbl = (t) => ({ head: t.head, rows: t.rows });
  const scheme = [
    ['Class year', 'First-year, Sophomore, Junior', 0, 'Seniors have no category to choose, so the set is not exhaustive. No categories overlap, so it is mutually exclusive.'],
    ['Hours of sleep last night', '0 to 4, 4 to 6, 6 to 8, 8 or more', 1, 'Someone who slept exactly 4, 6, or 8 hours fits two categories, so the set is not mutually exclusive. Every possible amount is covered, so it is exhaustive.'],
    ['Main way of getting to campus', 'Walk, Bike, Car', 0, 'Students who take a bus or train have no category, so the set is not exhaustive. The listed categories do not overlap.'],
    ['Age', 'Under 18, 18 to 24, 25 to 34, 35 or older', 3, 'Every age fits exactly one category, so the set is both exhaustive and mutually exclusive.'],
    ['Number of roommates', '0, 1 to 2, 2 to 3, 5 or more', 2, 'A student with 2 roommates fits two categories (not mutually exclusive), and a student with 4 roommates fits none (not exhaustive).'],
    ['Time on social media yesterday', 'Less than 1 hour, 1 to 3 hours, More than 3 hours', 3, 'Every amount of time fits exactly one category, so the set is both exhaustive and mutually exclusive.'],
    ['Favorite music genre', 'Pop, Hip-hop, Country, Rock', 0, 'Fans of any other genre have no option, so the set is not exhaustive. Adding "Other" would fix it.'],
    ['Weekly hours at a job', '0 to 10, 10 to 20, 20 or more', 1, 'Someone who works exactly 10 or 20 hours fits two categories, so the set is not mutually exclusive. Every number of hours is covered.'],
    ['GPA', 'Below 2.0, 2.0 to 2.99, 3.5 to 4.0', 0, 'A GPA between 3.0 and 3.49 has no category, so the set is not exhaustive. The listed ranges do not overlap.'],
    ['Number of siblings', '0 to 1, 1 to 2, 4 or more', 2, 'One sibling fits two categories (not mutually exclusive), and three siblings fit none (not exhaustive).']
  ];
  const SCHEME_LABELS = ['Not exhaustive', 'Not mutually exclusive', 'Both problems', 'Neither problem'];
  const units = [
    ['A researcher compares the average class size at 40 colleges.', 'Colleges', ['Classes', 'Students', 'Professors'], 'college'],
    ['A survey asks 500 students how many hours they sleep.', 'Students', ['Hours', 'Surveys', 'Colleges'], 'student'],
    ['A study compares voter turnout across New Jersey\'s 21 counties.', 'Counties', ['Voters', 'Elections', 'States'], 'county'],
    ['A researcher codes 300 TikTok videos for whether they contain health misinformation.', 'Videos', ['Creators', 'Viewers', 'Health claims'], 'video'],
    ['A study records the tip amount on 1,200 restaurant receipts.', 'Receipts', ['Servers', 'Restaurants', 'Dollars'], 'receipt'],
    ['An analyst compares median rent across the 50 U.S. states.', 'States', ['Renters', 'Apartments', 'Dollars'], 'state']
  ];

  level({ title: 'Rows, Columns, Cells', mod: '1.2', skill: 'Read a data table, name its parts, and test a coding scheme.' }, [
    () => { const t = makeTable(); return num('How many observations are in this data table?', t.k, 'An observation is one row: all the information collected for a single unit of analysis. This table has ' + t.k + ' rows of data. The header row names the variables and is not an observation.', { table: tbl(t), tol: 0 }); },
    () => { const t = makeTable(); return num('How many variables are in this data table?', t.head.length, 'A variable is a characteristic that can vary across observations, and each one is a column. Every column counts, including name, so there are ' + t.head.length + '.', { table: tbl(t), tol: 0 }); },
    () => {
      const t = makeTable(4, 5), c = pick(t.cols), ci = t.head.indexOf(c), r = pick(t.rows), kind = pick([0, 1, 2, 3]);
      const L = ['Observation', 'Variable', 'Value', 'Coding scheme'];
      const qs = ['The entire row for ' + r[0] + ' is a(n)...', 'The column ' + c + ' is a(n)...', 'The single entry "' + r[ci] + '" in ' + r[0] + '\'s row is a(n)...', 'For the variable ' + c + ', the complete set of possible entries (' + COLS[c].join(', ') + ') is the...'];
      return cat(qs[kind], L[kind], L, 'An observation is a row. A variable is a column. A value is the entry in one cell. The coding scheme is the complete set of categories or values a variable can take.', { table: tbl(t) });
    },
    () => {
      const t = makeTable(5, 7), c = pick(t.cols), ci = t.head.indexOf(c), r = pick(t.rows);
      const wr = shuffle(COLS[c].filter((v) => v !== r[ci])).slice(0, 3);
      return mc('What is ' + r[0] + '\'s value for ' + c + '?', r[ci], wr, 'Find the row for ' + r[0] + ' (the observation) and the column ' + c + ' (the variable). The cell where they meet holds the value: ' + r[ci] + '.', { table: tbl(t) });
    },
    () => { const [v, cats, i, why] = pick(scheme); return cat('A survey question about "' + v + '" offers these categories:\n\n' + cats + '\n\nWhat is wrong with this coding scheme?', SCHEME_LABELS[i], SCHEME_LABELS, 'Exhaustive means every observation fits some category. Mutually exclusive means no observation fits more than one. ' + why); },
    () => pick([
      () => mc('A survey asks for "favorite streaming service" with the categories Netflix, Hulu, and Max. Which change makes the categories exhaustive?', 'Add an "Other" category', ['Let students pick two services', 'Remove the Max category', 'Rename the variable'], 'Exhaustive means every observation fits some category. A student whose favorite is YouTube has nowhere to go until "Other" is added.'),
      () => mc('A survey lists age categories as 18 to 25, 25 to 35, and 35 or older. Which change makes them mutually exclusive?', 'Use 18 to 24, 25 to 34, and 35 or older', ['Add a category called "Other"', 'Add a category for under 18', 'Let people check every box that fits'], 'Mutually exclusive means no observation fits more than one category. As written, a 25-year-old and a 35-year-old each fit two. Adding "under 18" would help make the set exhaustive, which is a different problem.')
    ])(),
    () => { const [s, r, w, one] = pick(units); return mc(s + ' What is the unit of analysis?', r, w, 'The unit of analysis is the entity being studied, the thing each row of the data table represents. Here each observation is one ' + one + '.'); },
    () => { const r = pick([48, 120, 250, 312, 1500]), c = rnd(4, 15), askObs = Math.random() < 0.5; return num('A dataset has ' + QZ.commas(r) + ' rows of data and ' + c + ' columns. How many ' + (askObs ? 'observations' : 'variables') + ' does it contain?', askObs ? r : c, 'In a data table, rows are observations and columns are variables. So there are ' + QZ.commas(r) + ' observations and ' + c + ' variables.', { tol: 0 }); },
    () => cat('In a tidy data table, each row is a(n) ___ and each column is a(n) ___.', 'Observation; variable', ['Observation; variable', 'Variable; observation', 'Value; variable', 'Observation; value'], 'Tidy data are in tabular format: columns are variables and rows are observations. Each cell holds one observation\'s value on one variable.')
  ]);

  /* ============ LEVEL 3: Module 1.2 (sampling, inference, codebooks, RStudio) ============ */
  const popSamp = [
    { c: 'A college wants to know how its 7,000 undergraduates feel about extending library hours. It surveys 200 randomly selected undergraduates.', pop: 'All 7,000 undergraduates', samp: 'The 200 undergraduates surveyed', d: ['The library staff', 'Undergraduates who want longer hours'] },
    { c: 'A streaming company wants to learn the average watch time of its 2 million subscribers. It pulls the viewing records of 5,000 randomly chosen subscribers.', pop: 'All 2 million subscribers', samp: 'The 5,000 subscribers whose records were pulled', d: ['The shows on the platform', 'Subscribers who watch every day'] },
    { c: 'A town with 12,000 registered voters is deciding on a new park. A pollster calls 400 randomly selected registered voters.', pop: 'All 12,000 registered voters', samp: 'The 400 voters who were called', d: ['Everyone who uses the park', 'Voters who support the park'] }
  ];
  const paramStat = [
    ['The average GPA of every student enrolled at a college is 3.21.', 0], ['In a survey of 150 students at a college, 62% say they skip breakfast.', 1],
    ['A registrar calculates the percentage of all enrolled students who commute, using every student\'s record.', 0], ['A poll of 1,000 likely voters finds that 48% support a ballot measure.', 1],
    ['Forty randomly chosen dorm rooms have a mean temperature of 71 degrees.', 1], ['The mean age of all 100 U.S. senators is computed from every senator\'s birthday.', 0]
  ];
  const SYMS = [['the population mean', 'μ'], ['the sample mean', 'x̄'], ['the population standard deviation', 'σ'], ['the sample standard deviation', 's'], ['the sample size', 'n'], ['the population proportion', 'P'], ['the sample proportion', 'p̂']];
  const symScenes = [['The mean class size for all classes at a college is 23 students.', 'μ', 'This mean describes every class (the population), so it is a parameter: μ.'], ['In a survey of 200 seniors, the mean SAT score is 1170.', 'x̄', 'This mean comes from a sample of 200, so it is a statistic: x̄.'], ['Among all students at a college, 94% own a smartphone.', 'P', 'This proportion describes all students (the population), so it is the parameter P.'], ['In a sample of 80 students, 55% own a tablet.', 'p̂', 'This proportion comes from a sample, so it is the statistic p̂ (p-hat).']];
  const PANES = ['Source (top-left)', 'Console (bottom-left)', 'Environment (top-right)', 'Utilities (bottom-right)'];
  const paneQs = [['You want to write a script and save it to run again later. Which RStudio pane do you use?', 0], ['You type 2 + 2, press Enter, and R immediately returns 4. Which RStudio pane are you in?', 1], ['You run apple <- 1. Which RStudio pane now lists apple as an object with the value 1?', 2], ['Your code draws a histogram. Which RStudio pane displays the plot?', 3], ['You type ?mean to read the documentation. Which RStudio pane shows the help page?', 3], ['You run a line from your script and get an error message. Which RStudio pane displays it?', 1]];
  const STEPS = ['Import', 'Tidy', 'Transform', 'Visualize', 'Model', 'Communicate'];
  const stepQs = [['You load a .csv file into an R data frame.', 0], ['You rearrange a messy file so that columns are variables and rows are observations.', 1], ['You create a new variable and filter out some rows.', 2], ['You draw a histogram to look for patterns.', 3], ['You run a statistical test to answer a precise question.', 4], ['You write up your results so a general audience can follow them.', 5]];
  const srs = [[5, 25], [4, 20], [10, 40], [5, 50], [8, 40], [6, 24], [15, 60], [9, 30], [12, 40], [20, 50], [3, 30], [7, 35]];

  level({ title: 'Samples and Populations', mod: '1.2', skill: 'Tell populations from samples, pick the right symbol, and find your way around a codebook and RStudio.' }, [
    () => { const v = pick(popSamp), askPop = Math.random() < 0.5; return mc(v.c + '\n\nWhat is the ' + (askPop ? 'population' : 'sample') + '?', askPop ? v.pop : v.samp, [askPop ? v.samp : v.pop, ...v.d], 'The population is the entire group the study wants to draw conclusions about. The sample is the subset actually selected for analysis.'); },
    () => { const [s, i] = pick(paramStat); const L = ['Parameter', 'Statistic', 'Variable', 'Observation']; return cat(s + '\n\nThe number described here is a...', L[i], L, 'A parameter is a number that describes a population. A statistic is a number that describes a sample and is used to estimate a parameter. ' + (i === 0 ? 'This number is based on every member of the group, so it is a parameter.' : 'This number is based on a subset, so it is a statistic.')); },
    () => { if (Math.random() < 0.5) { const [d, s] = pick(SYMS); return mc('Which symbol represents ' + d + '?', s, sample(SYMS.map((x) => x[1]).filter((x) => x !== s), 3), 'Greek letters (μ, σ) and capital P describe populations, so they are parameters. x̄, s, and p̂ describe samples, so they are statistics. n is the sample size.'); } const [t, s, why] = pick(symScenes); return cat(t + '\n\nWhich symbol fits this number?', s, ['μ', 'x̄', 'P', 'p̂'], why + ' μ and P are parameters. x̄ and p̂ are statistics.'); },
    () => { const [n, N] = pick(srs); return num('A club has ' + N + ' members. You take a simple random sample of ' + n + ' members. What is each member\'s probability of being selected? Give a decimal.', n / N, 'In a simple random sample, every member of the population has an equal chance of being selected: ' + n + ' ÷ ' + N + ' = ' + fmt(n / N) + '.', { tol: 0.006, hint: 'Decimal, like 0.15' }); },
    () => pick([
      () => mc('A researcher wants a simple random sample of 100 students from a college. Which method produces one?', 'Randomly draw 100 names from the full enrollment list', ['Survey the first 100 students who walk into the library', 'Email every student and keep the first 100 who reply', 'Ask each professor to nominate a few students'], 'A simple random sample gives every member of the population an equal chance of being selected. Only a random draw from the full list does that.'),
      () => mc('A researcher surveys whoever happens to be in the library at noon. Why is this not a simple random sample of the student body?', 'Students elsewhere had no chance of being picked', ['The sample is too small to analyze', 'Libraries are not part of the population', 'A sample must include every student'], 'In a simple random sample, each member of the population has an equal chance of selection. Students who were not in the library at noon had no chance at all.')
    ])(),
    () => pick([
      () => mc('A survey of 300 students finds that 62% support a later start time for classes. Which statement communicates this finding appropriately?', 'We estimate that about 62% of students support a later start.', ['This proves that 62% of all students support a later start.', 'Exactly 62% of all students support a later start.', 'Any sample of students would show 62% support.'], 'Statistical inference uses a sample to draw conclusions about a population, and those conclusions always carry uncertainty. A different sample would give a slightly different number, so we do not make certain claims.'),
      () => mc('A researcher uses the average commute time of 400 sampled workers to draw a conclusion about all workers in the state. This process is called...', 'Statistical inference', ['Operationalization', 'Simple random sampling', 'Data cleaning'], 'Statistical inference is the process of using data from a sample to make generalizations about a broader population, such as estimating a population mean from a sample mean.')
    ])(),
    () => pick([
      () => mc('You open a dataset and see a variable named q7 with values 1 through 5. Where should you look to learn what a 5 means?', 'The codebook', ['The RStudio console', 'The last row of the data table', 'The Environment pane'], 'A codebook is a reference guide that describes each variable in a dataset: what it measures, how it is coded, its units, and how missing data are marked.'),
      () => mc('Which piece of information belongs in a codebook?', 'What each variable measures and how it is coded', ['Each respondent\'s answers, one row per person', 'The p-values from every test that was run', 'The R packages installed on your computer'], 'A codebook describes the variables, coding schemes, and values in a dataset. The answers themselves live in the data table.'),
      () => mc('Following this class\'s rule for naming variables, which name is best?', 'marital_status', ['Marital Status', 'MaritalStatus', 'marital status'], 'Variable names in this class are lower case, continuous strings: no capitals and no spaces.', { mono: true })
    ])(),
    () => { const [q, i] = pick(paneQs); return cat(q, PANES[i], PANES, 'Source (top-left) is where you write and save scripts. Console (bottom-left) runs commands and shows output and errors. Environment (top-right) lists the objects loaded in memory. Utilities (bottom-right) holds Files, Plots, Packages, and Help.'); },
    () => { const [q, i] = pick(stepQs); const opts = [STEPS[i], ...sample(STEPS.filter((_, k) => k !== i), 3)]; return cat(q + ' Which step of the tidyverse data science cycle is this?', STEPS[i], STEPS.filter((s) => opts.includes(s)), 'The cycle runs Import (load data), Tidy (columns are variables, rows are observations), Transform (filter, create variables, summarize), Visualize (plot to spot patterns), Model (answer precise questions), and Communicate (share results clearly).'); }
  ]);

  /* ============ LEVEL 4: Module 1.3 (variable types, operationalization) ============ */
  const TYPES = ['Continuous', 'Discrete', 'Ordinal', 'Nominal'];
  const TYPE_WHY = [
    'A continuous variable is quantitative and can take any value within a range, including fractions and decimals.',
    'A discrete variable is quantitative and takes separate, countable values (usually whole numbers) with nothing in between.',
    'An ordinal variable has categories with a meaningful order, but the gaps between categories are not necessarily equal.',
    'A nominal variable sorts observations into categories that have no natural order.'
  ];
  const vars = [
    ['Number of roommates a student has (0, 1, 2, 3)', 1], ['Time to walk to class, timed with a stopwatch (for example, 7.42 minutes)', 0], ['Class year (first-year, sophomore, junior, senior)', 2],
    ['Favorite streaming service (Netflix, Hulu, Max, other)', 3], ['Number of Instagram followers', 1], ['Height in centimeters (for example, 172.5)', 0],
    ['Agreement with "Campus parking is adequate" (strongly disagree to strongly agree)', 2], ['ZIP code of a student\'s hometown', 3, ' The digits are only labels. Averaging ZIP codes would be meaningless.'],
    ['Jersey number of a player on the soccer team', 3, ' The number is only a label. A player wearing 20 is not "twice" a player wearing 10.'], ['Coffee size ordered (small, medium, large)', 2],
    ['Number of classes a student is enrolled in', 1], ['Weight of a backpack in pounds (for example, 12.6)', 0], ['Residence hall a student lives in', 3], ['Hourly wage in dollars and cents', 0],
    ['Number of unread emails in an inbox', 1], ['Spice level ordered at a restaurant (mild, medium, hot)', 2], ['Phone operating system (iOS, Android, other)', 3], ['Distance from home to campus in miles (for example, 23.8)', 0]
  ];
  const concepts = [
    ['academic engagement', 'Number of class sessions attended this semester', ['A general sense of belonging to the campus community', 'How deeply a student cares about learning', 'The overall importance a student places on school']],
    ['social class', 'Annual household income in dollars', ['A family\'s overall standing in the community', 'The advantages a person inherits from parents', 'How comfortable a household feels financially']],
    ['political participation', 'Number of political activities done last year', ['A person\'s sense of civic duty', 'How much someone cares about politics', 'Commitment to the democratic process']],
    ['stress', 'Score on a ten-item stress questionnaire', ['The feeling of being overwhelmed by life', 'How much pressure a person is under', 'A person\'s overall mental load']],
    ['social media use', 'Daily minutes on social apps from a screen-time report', ['How central online life is to a person', 'A person\'s attachment to their phone', 'The role of social platforms in daily life']],
    ['food insecurity', 'Number of meals skipped last week because of cost', ['The experience of worrying about food', 'A household\'s general access to nutrition', 'How hard it is to afford groceries']]
  ];
  const conceptNot = [['Social class', ['Annual household income', 'Years of schooling completed', 'Number of books in the home']], ['Intelligence', ['Score on a standardized test', 'Number of puzzles solved in ten minutes', 'Grade point average']], ['Health', ['Resting heart rate', 'Number of doctor visits last year', 'Hours of sleep per night']], ['Popularity', ['Number of Instagram followers', 'Number of contacts in a phone', 'Party invitations received this month']]];
  const dummies = [
    { v: 'region', cats: ['North', 'South', 'East', 'West'], who: 'A respondent from the ' },
    { v: 'class_year', cats: ['first-year', 'sophomore', 'junior', 'senior'], who: 'A student who is a ' },
    { v: 'housing', cats: ['dorm', 'apartment', 'family home'], who: 'A student living in a(n) ' },
    { v: 'platform', cats: ['TikTok', 'Instagram', 'YouTube', 'Snapchat'], who: 'A student whose main platform is ' }
  ];

  level({ title: 'What Type of Variable?', mod: '1.3', skill: 'Classify variables as continuous, discrete, ordinal, or nominal, and turn concepts into measurable variables.' }, [
    () => { const v = pick(vars); return cat('What type of variable is this?\n\n' + v[0], TYPES[v[1]], TYPES, TYPE_WHY[v[1]] + (v[2] || '')); },
    () => { const v = pick(vars.filter((x) => !x[2])); return cat('What type of variable is this?\n\n' + v[0], TYPES[v[1]], TYPES, TYPE_WHY[v[1]]); },
    () => { const t = rnd(0, 3); const opts = [0, 1, 2, 3].map((k) => pick(vars.filter((v) => v[1] === k))[0]); return mc('Which of these variables is ' + TYPES[t].toLowerCase() + '?', opts[t], opts.filter((_, k) => k !== t), TYPE_WHY[t] + ' The other options are examples of the other variable types.'); },
    () => { const [c, r, w] = pick(concepts); return mc('A researcher wants to operationalize the concept of "' + c + '." Which option is a variable that could measure it?', r, w, 'Operationalization means defining a concept in a way that can be empirically observed. A concept is broad and abstract. A variable is specific, measurable, and takes different values across observations. Only one option here could be recorded as a column of data.'); },
    () => { const [r, w] = pick(conceptNot); return mc('Which of these is a concept rather than a variable?', r, w, 'A concept is an abstract idea that is not directly measurable. A variable is the specific, measurable form of a concept. The other options can each be recorded as a number for every observation.'); },
    () => mc(pick(['A researcher decides to measure "school spirit" by counting how many home games each student attended.', 'A researcher decides to measure "sleep quality" using the number of times a smartwatch records each person waking up.', 'A researcher decides to measure "religiosity" by asking how many times per month each person attends services.']) + ' This step is called...', 'Operationalization', ['Normalization', 'Statistical inference', 'Random sampling'], 'Operationalization is the process of translating an abstract concept into a concrete, measurable variable.'),
    () => { const d = pick(dummies), on = pick(d.cats), who = Math.random() < 0.5 ? on : pick(d.cats.filter((c) => c !== on)); const nm = on.toLowerCase().replace(/[^a-z]+/g, '_'); return num('The variable ' + d.v + ' has these categories: ' + d.cats.join(', ') + '. A dummy variable called ' + nm + ' is coded 1 = ' + on + ', 0 = otherwise.\n\n' + 'A respondent whose ' + d.v + ' is "' + who + '" gets what value on ' + nm + '?', who === on ? 1 : 0, 'A dummy variable equals 1 when the observation meets the named condition and 0 when it does not. This respondent ' + (who === on ? 'meets' : 'does not meet') + ' the condition "' + on + '."', { tol: 0, hint: '0 or 1' }); },
    () => mc('A dummy variable in a dataset is named employed. Following the usual naming convention, what does a value of 1 mean?', 'The person is employed', ['The person is not employed', 'The person has exactly one job', 'The person skipped the question'], 'We name dummy variables after the condition being met, so a 1 means the observation meets the condition and a 0 means it does not.'),
    () => mc('Education level (high school, some college, bachelor\'s, graduate degree) is ordinal rather than nominal because...', 'Its categories have a meaningful order', ['The gaps between its categories are equal', 'It can take decimal values', 'It has more than two categories'], 'Ordinal variables have categories with a natural order or ranking. Nominal categories have no order. Ordinal categories are not necessarily evenly spaced: the step from "some college" to "bachelor\'s" is not the same size as other steps.'),
    () => mc('Why is "number of siblings" discrete rather than continuous?', 'It only takes separate whole-number values', ['Its categories do not have any natural order', 'It was collected with a survey', 'Its values are usually small'], 'A discrete variable takes specific, separate values with nothing in between: nobody has 2.5 siblings. A continuous variable can take any value in a range, including fractions.')
  ]);

  /* ============ LEVEL 5: Modules 1.3-1.4 (R basics, ifelse) ============ */
  const intMeanVec = (n) => { for (;;) { const v = Array.from({ length: n }, () => rnd(1, 12)); if (sum(v) % n === 0) return v; } };
  const dset = [
    { v: 'year', cats: ['first_year', 'sophomore', 'junior', 'senior'], on: 'senior' },
    { v: 'housing', cats: ['commuter', 'dorm', 'apartment'], on: 'commuter' },
    { v: 'platform', cats: ['tiktok', 'instagram', 'youtube'], on: 'tiktok' },
    { v: 'party', cats: ['democrat', 'republican', 'independent'], on: 'independent' }
  ];
  function ifelseData() {
    const d = pick(dset), k = rnd(5, 7), names = sample(NAMES, k);
    let vals; do { vals = names.map(() => pick(d.cats)); } while (!vals.includes(d.on) || vals.every((x) => x === d.on));
    return { d, names, vals, table: { head: ['name', d.v], rows: names.map((n, i) => [n, vals[i]]) }, code: 'data$' + d.on + ' <- ifelse(data$' + d.v + ' == "' + d.on + '", 1, 0)' };
  }
  const syntax = [
    ['x <- c(2, 1, 5)', ['x <- c(2 1 5)', 'x <- (2, 1, 5)', 'x <- C(2, 1, 5)'], 'Vectors need commas between values and must be wrapped in c( ). Function names are case-sensitive, so C( ) is not the same as c( ).'],
    ['install.packages("readr")', ['install.packages(readr)', 'Install.Packages("readr")', 'install packages("readr")'], 'The package name goes in quotation marks inside install.packages(). R is case-sensitive, and function names cannot contain spaces.'],
    ['mean(scores)', ['Mean(scores)', 'mean(scores', 'mean scores'], 'Function names are lowercase and case-sensitive, and parentheses must come in pairs.'],
    ['my_variable <- 10', ['my variable <- 10', 'my-variable <- 10', '10 my_variable <-'], 'Object names cannot contain spaces, and R reads a hyphen as subtraction. Use underscores to separate words.'],
    ['library(readr)', ['library readr', 'Library(readr)', 'library(readr'], 'library() is lowercase, needs its parentheses, and both parentheses must be present.']
  ];
  const symQs = [
    ['In data$age, what does the $ do?', 'Pulls the variable age out of the data frame', ['Converts age into dollars', 'Assigns a new value to age', 'Marks the rest of that line as a comment for readers']],
    ['In x <- c(2, 1, 5), what does <- do?', 'Assigns the vector to the name x', ['Tests whether x is less than the vector', 'Removes the vector from x', 'Prints the vector to the console']],
    ['What does R do with a line that begins with #?', 'Ignores it, because it is a comment', ['Runs the line twice', 'Treats the line as the name of a package', 'Saves it as a new object']],
    ['In ifelse(data$party == "democrat", 1, 0), what does == do?', 'Tests whether two things are equal', ['Assigns "democrat" to party', 'Creates a variable called party', 'Joins the two pieces of text together']]
  ];

  level({ title: 'Talking to R', mod: '1.3-1.4', skill: 'Read basic R code, fix syntax errors, and build dummy variables with ifelse().' }, [
    () => { const v = intMeanVec(pick([4, 5])); return num('What does this code return?', mean(v), 'mean() adds the values and divides by how many there are: (' + v.join(' + ') + ') ÷ ' + v.length + ' = ' + sum(v) + ' ÷ ' + v.length + ' = ' + mean(v) + '.', { code: 'x <- c(' + v.join(', ') + ')\nmean(x)', tol: 0.001 }); },
    () => { const v = intMeanVec(pick([3, 4])); const k = pick([0, 1, 2]); const codes = ['sum(x)', 'length(x)', 'sum(x) / length(x)'], ans = [sum(v), v.length, mean(v)]; return num('What does the last line return?', ans[k], 'sum() adds up the values (' + sum(v) + ') and length() counts how many there are (' + v.length + '). Dividing one by the other gives the mean (' + mean(v) + ').', { code: 'x <- c(' + v.join(', ') + ')\n' + codes[k], tol: 0.001 }); },
    () => { const [r, w, why] = pick(syntax); return mc('Which line of R code runs without an error?', r, w, why, { mono: true }); },
    () => pick([
      () => mc('You installed the tidyverse package last week. Today you open RStudio, call ggplot(), and R says it could not find the function. What should you run?', 'library(tidyverse)', ['install.packages("tidyverse")', 'summary(tidyverse)', 'View(tidyverse)'], 'You install a package once per computer, but you must load it with library() every time you start a new R session.', { mono: true }),
      () => mc('How often do you need to run install.packages("haven") on your own computer?', 'Once', ['Every time you open RStudio', 'Once in every script', 'Before each function call'], 'install.packages() downloads a package to your computer, which you only need to do once. library() loads it, which you do every session.')
    ])(),
    () => { const x = ifelseData(), cnt = x.vals.filter((v) => v === x.d.on).length; return num('This code runs on the data table shown. How many rows get a 1 on the new variable?', cnt, 'ifelse() checks the condition for every row. It returns 1 where ' + x.d.v + ' equals "' + x.d.on + '" and 0 everywhere else. ' + cnt + ' row' + (cnt === 1 ? ' meets' : 's meet') + ' the condition.', { table: x.table, code: x.code, tol: 0 }); },
    () => { const x = ifelseData(), i = rnd(0, x.names.length - 1), a = x.vals[i] === x.d.on ? 1 : 0; return num('This code runs on the data table shown. What value does ' + x.names[i] + ' get on the new variable?', a, 'ifelse(test, value_if_true, value_if_false) returns the first value when the test is TRUE and the second when it is FALSE. For ' + x.names[i] + ', ' + x.d.v + ' is "' + x.vals[i] + '," so the test is ' + (a ? 'TRUE' : 'FALSE') + ' and the result is ' + a + '.', { table: x.table, code: x.code, tol: 0, hint: '0 or 1' }); },
    () => { const d = pick(dset), p = 'data$' + d.on + ' <- ifelse(data$' + d.v; return mc('Which command creates a dummy variable called ' + d.on + ' that equals 1 when ' + d.v + ' is "' + d.on + '" and 0 otherwise?', p + ' == "' + d.on + '", 1, 0)', [p + ' = "' + d.on + '", 1, 0)', p + ' == "' + d.on + '", 0, 1)', p + ' == ' + d.on + ', 1, 0)'], 'The test uses two equals signs (==) to compare. Text values go in quotation marks. The value for TRUE comes first, then the value for FALSE.', { mono: true }); },
    () => { const [q, r, w] = pick(symQs); return mc(q, r, w, '<- assigns ("gets"). $ pulls one variable out of a data frame. # starts a comment that R ignores. == tests whether two things are equal, while a single = sets an argument.'); },
    () => mc('Running this script from top to bottom gives the error "object \'scores\' not found." Why?', 'scores is used before it is created', ['mean() only works on data frames', 'The vector needs quotation marks', 'scores is not a valid name for an object'], 'Syntax in a script is sequential: each line assumes every line above it has already run. Line 1 asks for the mean of an object that does not exist until line 2.', { code: 'mean(scores)\nscores <- c(88, 92, 79)' }),
    () => { const cut = pick([15, 20, 30]), h = pick([cut - 8, cut - 3, cut + 4, cut + 10]); const a = h > cut ? '"full"' : '"part"'; return cat('A student has hours = ' + h + '. What value do they get on status?', a, ['"full"', '"part"', 'TRUE', String(h)], 'ifelse() tests the condition for each row. ' + h + ' > ' + cut + ' is ' + (h > cut ? 'TRUE, so it returns the first value, "full."' : 'FALSE, so it returns the second value, "part."'), { code: 'data$status <- ifelse(data$hours > ' + cut + ', "full", "part")', mono: true }); },
    () => mc('Which variable name follows the lower snake case style this class recommends?', 'hours_slept', ['HoursSlept', 'hours slept', 'Hours.Slept'], 'Lower snake case uses all lowercase letters with underscores between words. Short, clear, substantive names make scripts easier to read.', { mono: true })
  ]);
})();
