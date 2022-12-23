# SET - Significant Event Tools

### What is it?:
An application that gives you many inputs, and ouputs- all in the realm of event study. Determine if an event if statistically significant on the i.e stock price. Many papers are written on this, and their summaries are all unique to each case, but they almost all use the same methodology- in 400 papers, 79% used the same exepected returns model (the market model), and the only other model to choose from is the significance test (often only basic t-test). Hence a lot of papers aren't really being very innovative, and my application will bypass a lot of the man hours spent calculating and plotting these statistics. There is nothing on the market that compares to my applciation- it is a nice domain (not too nice) so I will almost definitly have users, and also the learning resources are horrible so I can branch into that. This tool is: innovative (not done), unique(

### But similar tools already exist!

1. Some R packages already exist, but a) no website exists b) still have to code (difficult) c) unsupported on newer versions of R d) cost money e) time
2. website with no need to code (ease of use) and people can copy the code generate for their parameters like on regexr
3. Many different parameters: event time frames, stock/index, significance test, expected returns model, CI. So very customisable to each person.
4. Can help people quickly determine if an event is significant for a specific metric. Or can use a date/ range of dates to find event and infer cause.

### (Further) Ideas:
- Can input parameters and will return every signifcant date in a time frame.
- Using every permutation of parameters and index/industry to create a massive dataset I could use a ML model (classification?) to determine which type of significane test + expected return model will produce the best results. This is actually a pretty massive thing because as mentioned before, many of these papers basically repoduce each others methods just on different industries/ countries and they always mention using different models -> would basically make dozens definitly, and potentially hundreds, of scientific papers redundant or reduce their use. And this is a paper that definitly WOULD be cited (infered from the papers I've read). MEGA!!!!!!!!
- Using ML (sentiment analysis) I can try to link a event from the news/social media to a specific date that shows significant results.
- Supposedly we can use bootstrap for event study

### Current Gameplan:
To add each feature one by one, then once I can run combinations of a few signficane tests and a few market models- i will begin to develop website with UI using ?shiny? for ease of use.
