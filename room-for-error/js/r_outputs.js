/* R output for the t-test, ANOVA and regression questions. Made once from simulated data so every number in a question agrees with the output shown. */
const R_OUT = {
 "T": [
  {
   "y": "sleep_hours",
   "g": "housing",
   "g1": "commuter",
   "g2": "resident",
   "m1": 6.24,
   "m2": 6.98,
   "p": 0.000483458231649169,
   "lo": -1.149,
   "hi": -0.332,
   "unit": "hours",
   "out": "\tWelch Two Sample t-test\n\ndata:  sleep_hours by housing\nt = -3.589, df = 119.36, p-value = 0.0005\nalternative hypothesis: true difference in means is not equal to 0\n95 percent confidence interval:\n -1.149 -0.332\nsample estimates:\nmean in group commuter mean in group resident\n                  6.24                   6.98"
  },
  {
   "y": "screen_time",
   "g": "job",
   "g1": "employed",
   "g2": "not_employed",
   "m1": 5.17,
   "m2": 5.93,
   "p": 0.015628320649200077,
   "lo": -1.375,
   "hi": -0.147,
   "unit": "hours",
   "out": "\tWelch Two Sample t-test\n\ndata:  screen_time by job\nt = -2.452, df = 120.54, p-value = 0.0156\nalternative hypothesis: true difference in means is not equal to 0\n95 percent confidence interval:\n -1.375 -0.147\nsample estimates:\nmean in group employed mean in group not_employed\n                  5.17                       5.93"
  },
  {
   "y": "exam_score",
   "g": "section",
   "g1": "evening",
   "g2": "morning",
   "m1": 77.79,
   "m2": 81.16,
   "p": 0.10637331150731717,
   "lo": -7.497,
   "hi": 0.741,
   "unit": "points",
   "out": "\tWelch Two Sample t-test\n\ndata:  exam_score by section\nt = -1.636, df = 68.90, p-value = 0.1064\nalternative hypothesis: true difference in means is not equal to 0\n95 percent confidence interval:\n -7.497 0.741\nsample estimates:\nmean in group evening mean in group morning\n                77.79                 81.16"
  },
  {
   "y": "coffee_spending",
   "g": "meal_plan",
   "g1": "no_plan",
   "g2": "plan",
   "m1": 14.4,
   "m2": 11.54,
   "p": 0.0050627454222736765,
   "lo": 0.882,
   "hi": 4.838,
   "unit": "dollars",
   "out": "\tWelch Two Sample t-test\n\ndata:  coffee_spending by meal_plan\nt = 2.871, df = 93.17, p-value = 0.0051\nalternative hypothesis: true difference in means is not equal to 0\n95 percent confidence interval:\n 0.882 4.838\nsample estimates:\nmean in group no_plan mean in group plan\n                14.40              11.54"
  },
  {
   "y": "study_hours",
   "g": "athlete",
   "g1": "athlete",
   "g2": "non_athlete",
   "m1": 10.88,
   "m2": 13.15,
   "p": 0.012648461634149368,
   "lo": -4.036,
   "hi": -0.501,
   "unit": "hours",
   "out": "\tWelch Two Sample t-test\n\ndata:  study_hours by athlete\nt = -2.559, df = 70.03, p-value = 0.0126\nalternative hypothesis: true difference in means is not equal to 0\n95 percent confidence interval:\n -4.036 -0.501\nsample estimates:\nmean in group athlete mean in group non_athlete\n                10.88                     13.15"
  },
  {
   "y": "stress_score",
   "g": "pet",
   "g1": "no_pet",
   "g2": "pet",
   "m1": 23.5,
   "m2": 20.85,
   "p": 0.03415416655614446,
   "lo": 0.202,
   "hi": 5.089,
   "unit": "points",
   "out": "\tWelch Two Sample t-test\n\ndata:  stress_score by pet\nt = 2.152, df = 86.49, p-value = 0.0342\nalternative hypothesis: true difference in means is not equal to 0\n95 percent confidence interval:\n 0.202 5.089\nsample estimates:\nmean in group no_pet mean in group pet\n               23.50             20.85"
  },
  {
   "y": "texts_per_day",
   "g": "phone",
   "g1": "android",
   "g2": "iphone",
   "m1": 62.1,
   "m2": 64.28,
   "p": 0.6677421188622291,
   "lo": -12.267,
   "hi": 7.896,
   "unit": "texts",
   "out": "\tWelch Two Sample t-test\n\ndata:  texts_per_day by phone\nt = -0.431, df = 90.35, p-value = 0.6677\nalternative hypothesis: true difference in means is not equal to 0\n95 percent confidence interval:\n -12.267 7.896\nsample estimates:\nmean in group android mean in group iphone\n                62.10                64.28"
  }
 ],
 "A": [
  {
   "y": "sleep_hours",
   "g": "class_year",
   "ssb": 19.6,
   "ssw": 163.3,
   "p": 0.003250891617256387,
   "out": "             Df  Sum Sq Mean Sq F value  Pr(>F)\nclass_year    3    19.6    6.53   4.837  0.0033 **\nResiduals   121   163.3    1.35"
  },
  {
   "y": "stress_score",
   "g": "school",
   "ssb": 460.4,
   "ssw": 3701.1,
   "p": 0.0007830132423637242,
   "out": "             Df  Sum Sq Mean Sq F value  Pr(>F)\nschool        2   460.4  230.20   7.589  0.0008 ***\nResiduals   122  3701.1   30.34"
  },
  {
   "y": "screen_time",
   "g": "platform",
   "ssb": 57.3,
   "ssw": 347.6,
   "p": 0.0001427786534997723,
   "out": "             Df  Sum Sq Mean Sq F value  Pr(>F)\nplatform      2    57.3   28.65   9.566  0.0001 ***\nResiduals   116   347.6    3.00"
  },
  {
   "y": "gpa",
   "g": "study_spot",
   "ssb": 0.08,
   "ssw": 21.38,
   "p": 0.8115242852826627,
   "out": "             Df  Sum Sq Mean Sq F value  Pr(>F)\nstudy_spot    2    0.08    0.04   0.209  0.8115 \nResiduals   107   21.38    0.20"
  },
  {
   "y": "weekly_spending",
   "g": "commute",
   "ssb": 3730.3,
   "ssw": 43029.1,
   "p": 0.018416522109889283,
   "out": "             Df  Sum Sq Mean Sq F value  Pr(>F)\ncommute       3  3730.3 1243.43   3.468  0.0184 *\nResiduals   120 43029.1  358.58"
  }
 ],
 "L": [
  {
   "y": "exam_score",
   "x": "hours_studied",
   "a": 62.838,
   "b": 2.351,
   "pb": 4.2821438509932554e-17,
   "r2": 0.5301,
   "xlo": 0,
   "xhi": 12,
   "xunit": "hour",
   "yunit": "points",
   "xname": "hours studied",
   "yname": "exam score",
   "out": "Call:\nlm(formula = exam_score ~ hours_studied, data = data)\n\nCoefficients:\n               Estimate Std. Error t value Pr(>|t|)\n(Intercept)      62.838      1.510   41.61   <2e-16 ***\nhours_studied     2.351      0.228   10.30   <2e-16 ***\n\nResidual standard error: 8.05 on 94 degrees of freedom\nMultiple R-squared:  0.5301,\tAdjusted R-squared:  0.5251\nF-statistic: 106 on 1 and 94 DF,  p-value: <2e-16"
  },
  {
   "y": "gpa",
   "x": "hours_gaming",
   "a": 3.377,
   "b": -0.022,
   "pb": 1.1445344698698429e-05,
   "r2": 0.1511,
   "xlo": 0,
   "xhi": 25,
   "xunit": "hour",
   "yunit": "GPA points",
   "xname": "weekly hours of gaming",
   "yname": "GPA",
   "out": "Call:\nlm(formula = gpa ~ hours_gaming, data = data)\n\nCoefficients:\n              Estimate Std. Error t value Pr(>|t|)\n(Intercept)      3.377      0.070   48.41   <2e-16 ***\nhours_gaming    -0.022      0.005   -4.58 1.14e-05 ***\n\nResidual standard error: 0.367 on 118 degrees of freedom\nMultiple R-squared:  0.1511,\tAdjusted R-squared:  0.1439\nF-statistic: 21.01 on 1 and 118 DF,  p-value: 1.14e-05"
  },
  {
   "y": "sleep_hours",
   "x": "caffeine_cups",
   "a": 7.644,
   "b": -0.309,
   "pb": 6.180805250608341e-06,
   "r2": 0.2125,
   "xlo": 0,
   "xhi": 6,
   "xunit": "cup",
   "yunit": "hours",
   "xname": "daily cups of coffee",
   "yname": "nightly hours of sleep",
   "out": "Call:\nlm(formula = sleep_hours ~ caffeine_cups, data = data)\n\nCoefficients:\n               Estimate Std. Error t value Pr(>|t|)\n(Intercept)       7.644      0.238   32.10   <2e-16 ***\ncaffeine_cups    -0.309      0.064   -4.82 6.18e-06 ***\n\nResidual standard error: 1.02 on 86 degrees of freedom\nMultiple R-squared:  0.2125,\tAdjusted R-squared:  0.2034\nF-statistic: 23.21 on 1 and 86 DF,  p-value: 6.18e-06"
  },
  {
   "y": "weekly_spending",
   "x": "distance_miles",
   "a": 54.204,
   "b": 0.36,
   "pb": 0.16510537872098618,
   "r2": 0.0266,
   "xlo": 0,
   "xhi": 30,
   "xunit": "mile",
   "yunit": "dollars",
   "xname": "miles from campus",
   "yname": "weekly spending",
   "out": "Call:\nlm(formula = weekly_spending ~ distance_miles, data = data)\n\nCoefficients:\n                Estimate Std. Error t value Pr(>|t|)\n(Intercept)       54.204      4.040   13.42   <2e-16 ***\ndistance_miles     0.360      0.256    1.40   0.1651 \n\nResidual standard error: 18.1 on 72 degrees of freedom\nMultiple R-squared:  0.0266,\tAdjusted R-squared:  0.0131\nF-statistic: 1.967 on 1 and 72 DF,  p-value: 0.1651"
  },
  {
   "y": "stress_score",
   "x": "credits",
   "a": 10.015,
   "b": 0.818,
   "pb": 4.1409080044910943e-07,
   "r2": 0.2121,
   "xlo": 9,
   "xhi": 20,
   "xunit": "credit",
   "yunit": "points",
   "xname": "credits enrolled",
   "yname": "stress score",
   "out": "Call:\nlm(formula = stress_score ~ credits, data = data)\n\nCoefficients:\n             Estimate Std. Error t value Pr(>|t|)\n(Intercept)    10.015      2.287    4.38 2.76e-05 ***\ncredits         0.818      0.152    5.39 4.14e-07 ***\n\nResidual standard error: 5.16 on 108 degrees of freedom\nMultiple R-squared:  0.2121,\tAdjusted R-squared:  0.2048\nF-statistic: 29.07 on 1 and 108 DF,  p-value: 4.14e-07"
  },
  {
   "y": "rent",
   "x": "distance_miles",
   "a": 1179.279,
   "b": -23.788,
   "pb": 5.19068960362588e-11,
   "r2": 0.5037,
   "xlo": 0,
   "xhi": 12,
   "xunit": "mile",
   "yunit": "dollars",
   "xname": "miles from campus",
   "yname": "monthly rent",
   "out": "Call:\nlm(formula = rent ~ distance_miles, data = data)\n\nCoefficients:\n                Estimate Std. Error t value Pr(>|t|)\n(Intercept)     1179.279     20.879   56.48   <2e-16 ***\ndistance_miles   -23.788      2.999   -7.93 5.19e-11 ***\n\nResidual standard error: 87.5 on 62 degrees of freedom\nMultiple R-squared:  0.5037,\tAdjusted R-squared:  0.4957\nF-statistic: 62.92 on 1 and 62 DF,  p-value: 5.19e-11"
  }
 ]
};
