# Contributing to COVID-19 Dashboard 🤝
🎊 Firstly, thank you for giving your time to contribute to the Dashboard. 🎊

What follows is a simple guide on how to contribute in a way that makes it easy for everyone.  
Contributions are made only on Github, through *issue* and then *pull request*.

# How to contribute
To contribute to Jobtimize, come and discuss it first by creating an **issue** of the changes you wish to make.  
Please note that all contributions, at any level, are appreciated and everyone is free to discuss. Thus we have a code of conduct to follow for healthy interaction in the project.

There are generally 3 types of contributions: translation, bug and new feature

## Translation
**You have to fork the repository first** and implement translation.  
*Otherwise, contact me and we'll discuss it (if you don't have a Github account for example).*

### How to fork repository
1. The best way to work is to fork the [repository](https://github.com/Lrakotoson/Covid-19), ideally on a fork of the master branch if it is an independent feature or a specific branch for a development associated with it.  

2. Clone your fork from the repository to your local storage. Keep it synchronized with the original repository in case of major changes using an upstream: [Learn more](https://help.github.com/en/github/getting-started-with-github/fork-a-repo).
```bash
$ git clone git@github.com:USERNAME/Covid-19.git
```

3. Create a specific branch for your `feature` where you will implement it. Creating a branch allows you to perform tests without the risk of affecting the main program.
```bash
$ git pull
$ git checkout -b my-translation
```

### Implement translation
To provide a dynamic translation system, the Dashboard uses **csv files** to translate from French into another language. There is one csv file for one language. In this file, there are two columns, in French and in the language.

1. To translate, make a copy of the `translation_blank.csv` file and replace `blank` with the **2-letter code** of the language.  
Then, open the file in your favorite editor, add to the first line the 2-letter language code and translate in the second column without modifying the first one.

2. Commit and Push your translation.  
Finally, make a pull request to the original Covid-19 repository [this way](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request-from-a-fork)  
Please leave checked the option
- [x] Allow edits from maintainers
```bash
$ git add translation-files
$ git commit
$ git push origin my-translation
```

*(For thos who test)*

3. If you run the Dashboard with your translation, it may not work because there is several options depending on your translation to be modified if your country does not appear before your contribution.  
Do not worry, I can implement this quickly after you submit your translation.

You can even give me the list of countries that speak the language you just implemented when you send your translations.  
I'll take care of the rest!

## Bug report
**Bugs must be treated as a priority.**  
When creating the issue, please describe the bug with an appropriate title. A template is prewritten to help explain the error encountered.

If you also want to help fix this bug, check  
- [x] "I'd like to fix this bug"

I will try to answer you as soon as possible.

## Feature request
Several contributions are grouped in this.  
Please add a clean title to the issue you opened. We will discuss your feature idea.


