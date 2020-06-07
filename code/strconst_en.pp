unit strconst_en;

{$mode objfpc}{$H+}
{
    Commandoo Program: Helper application for Linux commands / CLI
    Copyright (C) 2017 Julius Heinrich Ludwig Schön / Ronald Michael Spicer
    created by Julius Schön / R. Spicer
    Foto.TimePirate.org / TimePirate.org / PaganToday.TimePirate.org
    Julius@TimePirate.org

        This program is free software: you can redistribute it and/or modify
        it under the terms of the GNU General Public License as published by
        the Free Software Foundation, version 3 of the License.

        This program is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY; without even the implied warranty of
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        GNU General Public License for more details.

        You should have received a copy of the GNU General Public License
        along with this program.  If not, see <http://www.gnu.org/licenses/>.

}
interface

uses
  Classes, SysUtils;

const
  //ccapTabSelectedy = '◉ %s ◉';  ◆  ◢  ◣
  ccapTabSelected = '◢  %s  ◣';
  ccapTabUnSelected = ' %s ';


resourceString

//======================================================
//NOTE: Single "+ LineEnding"'s are important. I use them either for emphasis, or, more importanly,
//to limit the width of text designated for hints. If you don't limit the line for a hint it spreads across
//the screen and becomes almost unreadable. So hint text is made of short lines that require a carriage return.
//======================================================

  ccapTabCommands = '  Commands  ';//'Cmd''s';
  ccapTabFavorites = '  Favorites  ';//'Favs';
  ccapTabKeyWords = '  KeyWords  ';//'KeyW';
  ccapTabSearch = '  Search  ';//'Search';
  ccapTabProcesses = '==>  Detached Processes:  ';//'Proc''s';

  cstrCmdDisplayObjects = '>>         ↑ Commands   ---   ↓ Command Lines';

  ccapError = 'Error';
  ccapSuccess = 'Success';
  ccapInstructions = 'Instructions';
  cCantCopy = 'Could not copy "%s" to "%s".';
  //cCantCopyNotFound = '"%s" not found!';
  cFileNotExist = 'File "%s" does not exist.';
  cmsgAdvancedOptions = '< %s >';

  cAboutLine = 'commandoo   Version: %s';
  cAboutBDLine = 'DB Version: %s';
  cAboutInstalled = 'Program installed in:   %s';
  cAboutIni = 'Settings files (%s, %s & %s) in:   %s';
  cAboutLanguage = 'Language (.po) files in:   %s';
  cAboutFormSettings = 'Program Settings located in: %s';
  cAboutGitHub = 'This is open source software, the source code can be found on GitHub at: '
                 + LineEnding
                 + '      https://github.com/Juuliuus/commandoo'
                 + LineEnding + LineEnding
                 + 'Release Notes and download instructions can be seen in the '
                 + 'README file there.';
  cEmail = 'Comments etc. to email:     Commandoo@TimePirate.org';
  cContribute = 'Contributions of any amount you want can be given to Juuliuus@me.com on PayPal. '
                 + 'Another way to contribute, if you are bi-lingual, is to do a translation for commandoo. '
                 + 'It''s easy-ish...email me if you are interested. ';

  cInputCantBeBlank = 'Text cannot be blank';

  ccapChangeLangDoReset = 'Reset "Show No More" Dialogs?';
  cmsgChangeLangDoReset =
    'Do you want to reset all the "show no more" dialogs so you can read '
    + 'them in the new language?';

  cGNU_GPL =
    'Copyright (C) 2017 Julius Heinrich Ludwig Schön / Ronald Michael Spicer '
    + LineEnding
    + 'Created by Julius Schön / R. Spicer '
    + LineEnding
    + 'https://www.TimePirate.org'
    + LineEnding
    + 'https://Foto.TimePirate.org'
    + LineEnding
    + 'https://PaganToday.TimePirate.org'
    + LineEnding + LineEnding
    + 'Email: Commandoo@TimePirate.org'
    + LineEnding + LineEnding
    + 'Commandoo Program: Helper application for Linux commands / CLI '
    + LineEnding + LineEnding
    + 'This program is free software: you can redistribute it and/or modify '
    + 'it under the terms of the GNU General Public License as published by '
    + 'the Free Software Foundation, version 3 of the License. '
    + LineEnding + LineEnding
    + 'This program is distributed in the hope that it will be useful, '
    + 'but WITHOUT ANY WARRANTY; without even the implied warranty of '
    + 'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the '
    + 'GNU General Public License for more details. '
    + LineEnding + LineEnding
    + 'You should have received a copy of the GNU General Public License '
    + 'along with this program.  If not, see <http://www.gnu.org/licenses/>. '
    + LineEnding + LineEnding
    ;

  cmsgFirstLocalRunAbout = '<this is the message one sees from a First run of the program>'
    + LineEnding + LineEnding
    ;
  cmsgFirstLocalRun =
    'Hi. This looks like a first run of commandoo.'
    + LineEnding + LineEnding
    + 'First, some orientation. HELP is written into commandoo. At the top/right of almost '
    + 'every window is a checkbox. Click it, or use alt-h, and then help will be shown as you '
    + 'move your mouse over all the controls. '
    + LineEnding + LineEnding
    + 'The UI is large with many controls. Use Ctrl-g to bring up a popup menu to easily move '
    + 'to the most important sections.'
    + LineEnding + LineEnding
    + 'Also at the top/right is the ABOUT button, this opens a window where you can view '
    + 'information about commandoo, an Introduction, Tips and Tricks, and more. '
    + LineEnding + LineEnding
    + 'Other "Language / Sprache / Idioma / Langue" can be installed in OPTIONS if you have '
    + 'a language (.po) file for it.'
    + LineEnding + LineEnding
    + 'If you are upgrading from commandoo v1.0.1, see the ABOUT window for instructions.'
    + LineEnding + LineEnding
    + 'Most all windows are re-sizeable in width, height, or both. And they remember their positions '
    + 'and sizes. '
    + LineEnding + LineEnding
    + 'Use the copy/save buttons to the right to store any text in this memo, if nothing is '
    + 'selected it will take the entire text, otherwise just what is selected. '
    + LineEnding + LineEnding
    + 'You may wonder why the shortcut keys are seemingly random...This is done so that they '
    + 'remain the same if you change languages. '
    + LineEnding + LineEnding
    + 'Be sure to check out the OPTIONS button, don''t forget to use the alt-h help there. '
    + LineEnding + LineEnding
    + 'A note about "ibus", the linux system used, principally, for asian character entry. '
    + 'In earlier linux versions a problem would crop up where when you typed the characters '
    + '"the" one would get "tthhee", ie., doubled, or just a "t". And there were other systemic problems. '
    + 'This is an ibus problem, not a commandoo problem. '
    + 'If it affects you check out the ibus solution in the ABOUT window.'
    + LineEnding + LineEnding + LineEnding //I want space here
    ;

  cmsgCommandooUpgrade =
    'If you upgraded commandoo from the original version 1.0.1...'
    + LineEnding + LineEnding
    + '...you will notice your customized databases aren''t shown! '
    + 'But it is easy to point to them in just a few clicks.'
    + LineEnding + LineEnding
    + 'This new commandoo is future-proofed (!?), but the old one was not. '
    + LineEnding + LineEnding
    + 'The easiest way is to go to options and use the "Base/DB folder" button to choose the folder where '
    + 'your old databases are. Confirm, close commandoo, and re-start. Voila, all is right with the world. '
    + LineEnding + LineEnding
    + 'The other easy way, only a bit more work, is to use the "Manage DB Profiles" button, and then use '
    + 'the new IMPORT function. You can then import your custom DB''s one at a time into the new config '
    + 'area. '
    + LineEnding + LineEnding
    + 'What, exactly, has changed? The old commandoo simply looked in the same folder where it was installed. '
    + 'The new commandoo queries the system for the system determined user config folder (usually .config). '
    + 'But, as you now know, this can be over-ridden as described. '
    + LineEnding + LineEnding
    + 'Would it be better to import which moves everything to the new config folder? It is entirely up to you, '
    + 'commandoo doesn''t =care= where they are, it just needs to =know= where they are. If there is any advantage '
    + 'to importing it would be that the final structure is in line with gnu/linux quidelines and, in that '
    + 'way, "future-proofed".'
    + LineEnding + LineEnding
    + 'By the way, if you import your DB''s, it does not destroy the original files, they will still be in '
    + 'their original location. This is true of any imported DB.'
    + LineEnding + LineEnding
    + 'By this time you probably have thought that this would be a neat way of keeping separate folders of '
    + 'DB''s for different purposes and simply using this DB path to move between them. Yeah, you can do that '
    + 'now. The only reason I don''t recommend this method is that you could start getting confused as to what '
    + 'is where, and start making edits in one folder that you meant to make in another.'
    + LineEnding
    ;


  cmsgIbus =
    'Possible "ibus" problem and solution if it affects you'
    + LineEnding + LineEnding
    + '"ibus" is a gnu/linux input method (ie., keyboard entries) that is installed in many linux''s by default.'
    + LineEnding + LineEnding
    + 'In some older versions of gnu/linux ibus CAN CAUSE PROGRAM MISBEHAVIOR, making commandoo unusable. The problem '
    + 'and solution are outlined below. You may want to use the save to file button '
    + 'below, so that you can open it separately from this window.'
    + LineEnding + LineEnding
    + '==================== '
    + LineEnding
    + 'The Problem: '
    + LineEnding
    + 'Typed characters are doubled in some cases (solution below) '
    + LineEnding
    + '==================== '
    + LineEnding + LineEnding
    + 'In August 2017 (first official release of commandoo), I tested commandoo in a '
    + 'fresh Debian 9 cinnamon desktop. I found that the GTK version had the problem of '
    + 'doubled characters. That is, you type "dog" and you get "ddoogg", etc. This may '
    + 'be a problem in Debian 8 as well. '
    + LineEnding + LineEnding
    + 'Later in October a fresh install of KDE desktop Debian 9 also had this problem '
    + 'in the QT version. '
    + LineEnding + LineEnding
    + 'This was, of course, quite annoying. '
    + LineEnding + LineEnding
    + 'The problem is due to the program "ibus". ibus is an input method utility. It is '
    + 'designed to allow asian country users to type in their native language '
    + 'characters/idiograms: chinese, japanese, etc. So if you are not in Asia the '
    + 'program is not essential. However, it IS installed BY DEFAULT in fresh '
    + 'english installs of Debian. '
    + LineEnding + LineEnding
    + 'After finally testing (and researching) this thoroughly I found that the problem '
    + 'is rooted in ibus, or at least it''s implementation. I can say this with confidence '
    + 'because while Lazarus programs have this problem it is ALSO A PROBLEM in KDE desktop '
    + 'environment (at least at this time, maybe it will be fixed??). '
    + LineEnding + LineEnding
    + 'On the fresh KDE install not only was commandoo affected but KDE windows '
    + '(like kdesudo) failed, one can not type into the input fields. It also affects typing '
    + 'in the KDE kicker when trying to find some installed program, only the first typed '
    + 'character was displayed. Web research showed that many people were having this problem '
    + 'with KDE and Debian 9 and ibus. '
    + LineEnding + LineEnding
    + 'You can see immediately if you are affected by this problem. Simply close this window '
    + 'and continue on into commandoo. Focus the command list and type alt-F to open the find dialog and type '
    + '"dog". If "dog" is the result then you do not have this problem. If it comes out only "d" '
    + 'or as "ddoogg" then you do have the problem and should apply the solution. '
    + LineEnding + LineEnding
    + 'When I discovered this back in August I recommended either removing ibus (if you are '
    + 'not typing asian characters) or a "Fix" I had found on the internet that seemed to work '
    + '(setting some environment variables in .bashrc). Further testing in October with KDE '
    + 'showed the .bashrc "solution" was not effective though it had worked for me in August '
    + 'with GTK. That turned out to be due to how GTK enironment variable is set deep in the '
    + 'IM (input method) configuration files. So the solution DID NOT work with QT. '
    + 'At the time I had intended to try QT also but then the problem of not being able to use '
    + 'the GUI sudo commands (see above) convinced me that the KDE debian 9 install was flawed '
    + 'and I didn''t bother. '
    + LineEnding + LineEnding
    + 'But, finally, in October I tried it again, and still KDE/PLasma wouldn''t work properly '
    + '(for example, one can not open synaptic package manager, while the password dialog shows '
    + 'you can''t type anything into it! Ouch.). '
    + LineEnding + LineEnding
    + 'So I resolved to figure out the issue, and I did. Solution follows. '
    + LineEnding + LineEnding
    + '==================== '
    + LineEnding
    + 'Solution: '
    + LineEnding
    + '==================== '
    + LineEnding + LineEnding
    + 'As I said, I initially, in part, recommended removing ibus from your system if you '
    + 'didn''t need it. '
    + LineEnding + LineEnding
    + 'And, as someone pointed out on the internet, to paraphrase: While ibus is now the '
    + 'default input method it doesn''t mean you have to use it. There are other im''s out '
    + 'there and you can set the system to use those (they also have to do with asian '
    + 'characters). And removing ibus should have no deleterious effects if you don''t need it. '
    + LineEnding + LineEnding
    + 'So there is still that "solution". If you are having the doubled character problems '
    + 'you can temporarily see the "fix" immediately. Simply type "ibus exit" in a terminal. '
    + 'Go back into commandoo and the doubled character problem will be gone. But this is '
    + 'temporary, only shutting ibus down for the current session. On your next login ibus '
    + 'will be restarted. '
    + LineEnding + LineEnding
    + 'So removal of ibus will work. But I resolved to make my system work WITH ibus '
    + 'installed. And so here is the new tested and recommended solution: '
    + LineEnding + LineEnding
    + 'There turns out to be a small utility called im-config with which you can disable '
    + 'the asian character sub-system. '
    + LineEnding + LineEnding
    + 'You need to run it as root in this format:  sudo im-config -c '
    + LineEnding + LineEnding
    + 'The -c opens it in an interactive console mode in a terminal window. '
    + LineEnding + LineEnding
    + 'It will first list what the system currently is. This is usually defaulting to ibus. '
    + LineEnding + LineEnding
    + 'It will ask if you want to continue (yes). '
    + LineEnding + LineEnding
    + 'Then a screen will appear with your options. Use arrow keys to move to NONE choice, '
    + 'press spacebar to select it, then tab to OK and enter. '
    + LineEnding + LineEnding
    + 'NOTE: if you run im-config again it STILL says ibus is automatic, but this is not '
    + 'true absolutely, just in "theory" I guess (I really think someone, Linus?, needs to '
    + 'look into this mess), because if you continue you will see that the choice is still '
    + '"none", which is correct. '
    + LineEnding + LineEnding
    + 'After this process logout/reboot and, voila, the system works. KDE works, I was able '
    + 'to open synaptic package manager, commandoo worked. The KDE Plasma Kicker worked. '
    + 'And ibus is still installed. '
    + LineEnding + LineEnding
    + 'I would apologize for the mess of this but it is the ibus team that should. '
    + LineEnding + LineEnding
    ;

  ccapIntro = 'Introduction';
  cmsgIntro =
    'Introduction:'
    + LineEnding
    + '============ '
    + LineEnding + LineEnding
    + 'Welcome to commandoo the Graphical User Interface (GUI) for the Linux '
    + 'Command Line Interface (CLI), perhaps better known as the "Terminal". '
    + LineEnding + LineEnding
    + 'It''s purpose is to give you a place to store the cool and/or useful '
    + 'command lines you find, discover, or create. Then, months later, when '
    + 'you need it again, it will be here, easy to find. '
    + LineEnding + LineEnding
    + 'It''s primary audience was intended for people new to Linux, but the features and '
    + 'usefulness of storing command lines could be of interest for veteran users too. '
    + LineEnding + LineEnding
    + 'You can copy the Command Line with a right click and paste into a Terminal to use...Or...'
    + LineEnding + LineEnding
    + '...you can run it from within this program. Commandoo is NOT a terminal replacement, '
    + 'but it''s been designed to be as useful as possible and running Command Line''s seemed like a '
    + 'nice feature. '
    + LineEnding + LineEnding
    + 'Other features: '
    + LineEnding + LineEnding
    + '** Commands and Command Lines can be marked as to how dangerous they are, and you can '
    + 'optionally set that a particular danger level (harmless, careful, caution, danger) needs '
    + 'confirmation before running. '
    + LineEnding + LineEnding
    + '** You can mark a command line to take input from you, before running. Not all Commands pay '
    + 'any attention to input (stdin) but for the ones that do this is kind of a cool feature. '
    + 'Think grep for instance. '
    + LineEnding + LineEnding
    + '** You can test your command lines while editing them. The only thing this program does NOT '
    + 'provide is a shell within which to run commands. So running commands from here like "cd", "pwd", "alias" '
    + 'and some others like these are, well, useless to run from here. But even these are included in the '
    + 'database for reference since the program can show the command''s help text for you. '
    + LineEnding + LineEnding
    + '** You can add any number of self defined keywords to a command and these, of course, are searchable. '
    + LineEnding + LineEnding
    + 'There is one other thing to know about the absence of a "true" shell: you can''t change directories (cd) '
    + 'to run a command in that directory. For instance, if you choose to add "git" to your Commands you need to use '
    + 'it with the "-C <path>" flag. This is true of any command like git, you will need to use pathing and appropriate '
    + 'flags to run it. And the last BIG difference: any command that expects input will NOT run properly because '
    + 'I haven''t figured out how to communicate (yet) with a running process in a shell-like manner waiting for input. '
    + 'These have to be run in a terminal. '
    + LineEnding + LineEnding
    + 'You can choose to use text based database files or sqlite db files. You can convert '
    + 'from one to the other. You can merge from one to the other. You can send individual Command Lines '
    + 'from your open DB and send it to another DB. You can import DB''s from other commandoo users. You can '
    + 'compare DB''s. You can mix and match all these functions to manipulate your DB''s at will. '
    + LineEnding + LineEnding
    + 'You can edit a Command and a CL at the same time. '
    + LineEnding + LineEnding
    + 'This is completely "open" software, the source code is on GitHub, see the link in the '
    + '"About" window. It is written in pascal (thank you FreePascal) with the Lazarus IDE (thank you Lazarus). '
    + LineEnding + LineEnding
    + 'Help is included in the program. Almost all windows have a checkbox in the upper right to "show '
    + 'hints on mouse-over". Check that and then move the mouse over the control you have a question '
    + 'about. Uncheck it when you''re done.'
    + LineEnding + LineEnding
    + 'A word of CAUTION:     Be careful out there. '
    + LineEnding + LineEnding
    + 'Linux comes with very powerful commands to use, really cool ones. But they can be dangerous '
    + 'if misused and/or misunderstood. As they say with great power comes great responsibility. '
    + LineEnding + LineEnding
    + 'If you don''t know whether a command is dangerous or not then you really '
    + 'need to educate yourself about that command. Hopefully this database will help you with that. '
    + 'I will try to be as thorough as possible in the database descriptions of a command''s merits. And, '
    + 'in the original database coming with this program, if I have marked a command line as harmless '
    + 'then it IS harmless you can be certain. If a command line is marked careful or higher then you '
    + 'may want to spend some time learning about that command so you understand WHY it is not completely harmless. '
    + LineEnding + LineEnding
    + 'Another word of CAUTION:  Backup your database files regularly. File locations can be seen in the "About" '
    + 'window. And now License Information: '
    + LineEnding + LineEnding
    + '%s'
    + '%s'
    ;

  cmsgTips =
    'TIPS:'
    + LineEnding
    + '===== '
    + LineEnding + LineEnding
    + 'Commands are entries that describe the command you are using. Commands >> CAN NOT BE RUN <<. '
    + 'Only Command Lines (CL''s) can be run, but you must add the command first. Then you add command lines to it. '
    + LineEnding + LineEnding
    + 'Before adding Command Lines you should edit your new Command and give it a threat level and so on. '
    + 'This is because any command lines you add will "inherit" those settings '
    + 'saving you some work. '
    + LineEnding + LineEnding
    + 'Ctrl-g anywhere to easily move to important controls.'
    + LineEnding + LineEnding
    + 'commandoo supports variables in Command Lines. Select the CL you want and go into edit mode. Then use '
    + 'the CL builder/helper button and you will be taken to a edit window that has buttons to allow you to enter various '
    + 'types of variables (strings, integer, real, and file/folder names).  '
    + LineEnding + LineEnding
    + 'Look through the provided database, there are examples of variables, Command Line input, etc. '
    + LineEnding + LineEnding
    + 'One cool options is "Alert". If this is checked then the program makes you look at the notes for that '
    + 'Command Line. This can be really useful if you need to be aware of something, or having done something, '
    + 'BEFORE you run that command line. Try it. '
    + LineEnding + LineEnding
    + 'If you want commandoo to act as a "launcher" be sure to check the "Child Proc" checkbox in the Command Line. '
    + 'Otherwise, it will (usually) return immediately, having not done anything. Say you are running Gimp or Blender, '
    + 'well sending it out to be run like "uname" does precisely that. It runs it, and then stops and kills it. The '
    + 'Child Proc setting says "hey, this is a GUI program, keep it open". '
    + LineEnding + LineEnding
    + '"Use Shell" attempts to let the system handle it and this usually works pretty well. It is necessary for '
    + 'processes that use more terminal like syntax (semicolons and such). commandoo can internally handle the basic '
    + 'command lines, but if yours doesn''t seem to work, try "Use Shell" and see if that does it. If not then '
    + 'that Command Line should only be run in a Terminal (term. only). '
    + LineEnding + LineEnding
    + 'If the Command Line is mission critical, complex, etc. you should probably only use it in a Terminal. commandoo '
    + 'is pretty good at performing most CL''s but IT IS NOT A TERMINAL, be aware of that and use your best judgement '
    + 'on where commandoo can/should be used and where a Terminal should be used. '
    + LineEnding + LineEnding
    + '"Wants Input" is pretty cool (I think). On Commands that accept input (like, for instance, grep) marking this '
    + 'option for a Command Line will ask you for input before running the Command Line. So for something like grep you '
    + 'can paste in some text you want to search. '
    + LineEnding + LineEnding
    + 'The "SuperUser" setting only marks the CL that it should/will be run as root in a terminal. Setting '
    + 'this adds a "<root> " at the beginning of the CL. Later if you need this command, copy it from commandoo''s '
    + 'right click menu, then when you paste it in a terminal it will fill the CL out for you with the proper superuser '
    + 'command, ie., <root> alias will be pasted as sudo alias. What <root> get''s replaced with is from a template '
    + 'you can set from OPTIONS. You can not / may not run sudo or su commands from within commandoo, they must be '
    + 'run in a terminal. I have had success, however, with pkexec which I use from commandoo to mount a ram drive. '
    + 'So try pkexec too. Success of running CL''s with pkexec permissions will depend on your system''s policies.'
    + LineEnding + LineEnding
    + 'Use DB profiles, why not? The provided database is of basic Linux commands. Now you can add more and more to this '
    + 'database as you like, fine. But you could also make specific databases for specific needs. This is another way '
    + 'to make commandoo useful for you. Maybe you want a GUI launcher database for all your programs. And another that '
    + 'just deals with hardware commands. Etc. Make commandoo yours. '
    + LineEnding + LineEnding
    + 'Almost every window in commandoo has a checkbox at upper right to show hints on mouse-over. Use it!! Lot''s of information '
    + 'there. When you are done reading, uncheck it to stop the hints. The detailed hints for the Commands and CL''s are '
    + 'only shown in edit mode, so be sure to go into the 2 edit modes to see those details. '
    + LineEnding + LineEnding
    + 'Heads UP: I ran into an odd problem with a couple of the more abstruse, esoteric Commands. When you add  '
    + 'a new Command the HELP and VERSION fields are filled in for you with the most common variants: "--help" and '
    + '"--version". Upon entering a new Command I always check that these are correct for that Command, sometimes they '
    + 'aren''t. Well, in a couple of cases running the "--help" threw me out of my session!! Surprise! I don''t know '
    + 'why, it is hard to reproduce. But, for this reason, I recommend you test the help and version commands in a '
    + 'TERMINAL WINDOW first. '
    + LineEnding + LineEnding
    ;

  cmsgOwnRisk =
    'The use of of this FREE as in FREEDOM software is at YOUR OWN RISK. While I strive for complete testing it '
    + 'has no implicit or explicit warranty or guarantee of being bug free. '
    + LineEnding + LineEnding
    + 'I hope you find it useful, enjoy. '
    + LineEnding + LineEnding
    ;

  ccapMultipleDisallowed = 'Already running';
  cmsgMultipleDisallowed = 'commandoo is already running and your OPTIONS setting disallows this.'
                        + LineEnding
                        ;
  ccapMulipleInstances = '  >>>>> %d COPIES OPEN!! Careful!!';
  cmsgMulipleInstances = '%d COPIES OPEN, Closing this window is highly recommended!! '
                        + 'Continue using it at your own risk with the understandking that '
                        + 'these windows know nothing about what the other(s) is doing and '
                        + 'there could be unintended / unanticipated consequences. Exclamation Point.'
                        + LineEnding + LineEnding
                        + 'Do you STILL want two (or more?!) commandoo windows open?????'
                        + LineEnding
                        ;

  cMsgHarmless = 'No changes to system or files';
  cMsgCareful = 'Can change system and/or files, know how to use';
  cMsgCaution = 'Probable system changes, research before use!';
  cMsgDanger = 'Can trash your system, research before use!!!';

  ccapListManagerNothingSelected = 'Nothing Selected';
  cmsgListManagerNothingSelected = 'You have not selected an item in the list.';
  ccapListManagerDuplicate = 'Duplicate %s';
  cmsgListManagerDuplicate = '%s "%s" is already in the list.';

  cDisplayUpdating = 'Updating %s';

  ccapPathCaption = 'Path to: "%s"';
  cmsgNotSpecified = '-???-';
  cmsgInvalidString = '< Invalid >';
  cmsgProfileString = 'Current DB:   %s%s %s';


  cmsgProcessSingular = 'Proc';
  cmsgProcessPlural = 'Procs';
  ccapDetachedProcesses = '%d  Running%s';

  ccapQuickRun = 'Command to Run';
  ccapSwitchDBNotAllowed = 'Can not Switch or Manage Database';
  cmsgSwitchDBNotAllowed = 'Can not Switch or Manage Database Profiles while editing.';
  ccapCurrentlyEditing = 'Currently Editing';
  cmsgCurrentlyEditing = 'Currently editing a Command or Command Line. This task is not allowed '
                         + 'while editing. Apply your editing changes, or cancel them.';

  ccapSwitchDBSave = 'Unsaved Data';
  cmsgSwitchDBSave = 'There is currently unsaved data. Do you want me to save it before switching the Database?';
  cmsgManageDBSave = 'You have unsaved edited items. You need to save your changes before entering management mode.';

  cmsgLimitInfinity = LineEnding + LineEnding
                       + '{It appears you are using /dev/[ zero|full|random|urandom ] in this command '
                       + LineEnding
                       + 'line. As a safety measure to prevent "runaway" processes input / output has '
                       + LineEnding
                       + 'been limited to %d characters. For the most common uses of these devices '
                       + LineEnding
                       + 'it should not affect your results. If you do want this command to run to finish '
                       + LineEnding
                       + 'using these devices change the limit in options or run the command in a terminal.} '
                       + LineEnding + LineEnding ;
  cmsgSudoSuProblem = Lineending + LineEnding
                      + '______________________'
                      + LineEnding
                      + 'It appears you are using "sudo" or "su" in your command line. This is '
                      + LineEnding
                      + 'not allowed as it is not secure! Run this command line through '
                      + LineEnding
                      + 'a terminal. Better is to remove the sudo (or su) and use the "run as SuperUser" option '
                      + LineEnding
                      + 'which helps you when copying and pasting using a ROOT template (look in OPTIONS). '
                      + 'You can also try "pkexec", which depends on system security profiles but can '
                      + 'give good results in some cases (test in terminal first)'
                      + LineEnding;

  ccapThreatLevelWarning = 'Allow Execution "%s"?';
  cmsgThreatLevelWarning = 'Threat Level: %s '
                           + LineEnding + LineEnding
                           + 'Do you want to allow the command line: '
                           + LineEnding + LineEnding
                           + '==> %s'
                           + Lineending + LineEnding
                           + 'which is marked -- %s -- to run? '
                           + Lineending + LineEnding + LineEnding
                           + '( These confirmations can be changed in "Options" )'
                           + LineEnding;

  cmsgDisallowedPhrase = 'A command may not have "%s" in the name.';
  cmsgEmptyString = 'A Command name not be blank.%s';
  ccapDisplayCaptionAndValue = '%s:  %s';
  ccapOverflow = 'Overflow from long line';

//hints
  cmsgDisplayPanels = 'This shows the current state of the selected '
                      + LineEnding
                      + '%s settings.'
                      + LineEnding + LineEnding
                      + 'If you focus on the Notes area and press '
                      + LineEnding
                      + 'Ctrl-F a text search dialog opens. '
                      + LineEnding + LineEnding
                      + 'And Ctrl-Shift-F or Ctrl-L will find the next '
                      + LineEnding
                      + 'find occurence. '
                      + LineEnding + LineEnding
                      + '<end>';
  cmsgEditButtons = 'Open the "%s" Edit panel to edit the %s settings.'
                      + LineEnding + LineEnding
                      + '<end>';

  cmsgEditOk = 'Confirm any changes you have made to the "%s".'
                      + LineEnding + LineEnding
                      + '<end>';

  cmsgEditCancel = 'Cancel the changes to the "%s".'
                      + LineEnding + LineEnding
                      + '<end>';

  cmsgThreatLevelsCommand =
    'In the commands it is really a convenience '
    + LineEnding
    + 'since you can not run Commands, only '
    + LineEnding
    + 'Command Lines. But if you set this properly '
    + LineEnding
    + 'on the Command then when you add a '
    + LineEnding
    + 'new Command Line it will inherit that threat '
    + LineEnding
    + 'level. '
    ;

  cmsgThreatLevelsCommandLine =
    'The Command line inherits the threat level '
    + LineEnding
    + 'of the Command when it is added. Depending '
    + LineEnding
    + 'on the flags used the threat level may '
    + LineEnding
    + 'be different for this command line. You '
    + LineEnding
    + 'can change it as you like. '
    ;

  cmsgThreatLevels =
    'This allows you to use the feature of '
    + LineEnding
    + '"Threat Levels". '
    + LineEnding + LineEnding
    + 'As you know some Linux commands can '
    + LineEnding
    + 'be quite dangerous.  Others should be used '
    + LineEnding
    + 'properly. '
    + LineEnding + LineEnding
    + 'Labeling the Command (and Command Lines) '
    + LineEnding
    + 'with an appropriate warning gives a heads '
    + LineEnding
    + 'up to the you the user to make sure you '
    + LineEnding
    + 'know what you are doing and have done it '
    + LineEnding
    + 'properly. '
    + LineEnding + LineEnding
    + 'In addition, in the Options you can specify '
    + LineEnding
    + 'which levels of threat should be confirmed '
    + LineEnding
    + 'before running. '
    + LineEnding + LineEnding
    + '%s '
    + LineEnding + LineEnding
    + '<end> ';

  cmsgHelpVersionInfoVersion = 'Version';
  cmsgHelpVersionInfoHelp = 'Help';

  cmsgHelpVersionInfo =
    'Uses the "%s" flag saved with this Command '
    + LineEnding
    + 'to display the Command''s %s. '
    + LineEnding + LineEnding
    + 'If this doesn''t work then the %s flag for '
    + LineEnding
    + 'this Command will need to be edited. '
    + LineEnding + LineEnding
    + 'This also will not work if the Command has '
    + LineEnding
    + 'a bad name. '
    + LineEnding + LineEnding
    + '<end> '
    ;

  //cmsgRootNoStartup = 'It not allowed to run the program the FIRST TIME as <ROOT>.   Halting.';
  cmsgPermissionsError = 'Could not write to "%s", apparent system permissions problem.   Halting.';
  ccapRootModeConfirmation = 'Confirm <ROOT> Mode';
  cmsgRootModeConfirmation =
     'Hi. You are running commandoo in <ROOT> mode. '
     + LineEnding + LineEnding
     + 'There is literally no good reason to do so and many good reasons not to do so. '
     + LineEnding + LineEnding
     + 'But I don''t want to tell you what to do.'
     + LineEnding + LineEnding
     + 'Do you STILL want to run commandoo in <ROOT> mode even though it''s somewhat dangerous??????? '
     + LineEnding + LineEnding
     ;

  ccapGeneralInformation = 'Information you may want to know';

  ccapMasterListEmpty = 'Master List is empty';
  cmsgMasterListEmpty = 'The Master %s List is empty. Can not search on it.';

  ccapSearchDesignedInDiffDataBase = 'Database Stamp MisMatch';
  cmsgSearchDesignedInDiffDataBase =
     'Loaded file was designed in "%s" Database Profile but you are currently in "%s" Profile. '
     + 'It is quite possible the search will not be valid in this database. Do you still want to load it? '
     ;

  cmsgSearchMayBeInvalid =
     'Changes have been made that can affect Searches. '
     + 'For this reason any current search results may be incorrect. You should probably '
     + 're-run the search(es).'
     ;

  ccapRootFileTest = 'ROOT template test...';
  ccapRootFileInfo = 'ROOT Copy/Paste template';
  cmsgRootFileInfo = 'The template requires a single "%s" in it so that the command line can be placed properly. ';


  ccapSudoSet = 'ROOT Privileges';
  cmsgNoGoodSudo = 'I`ve searched for "kdesudo" and "gksudo", the safe and secure '
                       + 'gnu/Linux methods of launching programs with ROOT privileges '
                       + 'but didn`t find them.'
                       + LineEnding + LineEnding
                       + 'So, if you want to run commands with ROOT privileges you will '
                       + 'need to set it manually in "OPTIONS".'
                      ;
  cmsgSudoSet = 'Your ROOT mode file was not set. I`ve set your ROOT mode privileges file to "%s". '
                + 'For most users this will be correct.'
                + LineEnding + LineEnding
                + 'If you want to change it do so in "OPTIONS".'
                + LineEnding + LineEnding
                + 'If running ROOT mode has no effect then you will NEED to change it to the proper file '
                + 'depending on your Linux distribution (Debian vs. ''Buntu) and the type of administrative '
                + 'setup (sudo vs sudo''ers vs su) it has.'
                      ;

  ccapRootDisallowed = 'ROOT running Disallowed';
  cmsgRootDisallowed =
    'The ROOT choice for a command line is simply a helper so that when a CL is copied and then pasted  '
    + 'into a terminal the proper ROOT template is used (ie., "sudo", "su -c", etc.) so that the CL can be '
    + 'run directly in the terminal. The template can be changed in OPTIONS.'
    + LineEnding + LineEnding
    + 'It is not allowed to run ROOT CL''s through commandoo, it simply isn''s safe. Use a terminal to '
    + 'run ROOT commands, or look into the other approved methods using "pkexec", or the admin:// option '
    + 'for files.'
    + LineEnding
    ;

  ccapCommandLineDisallowed = 'Command Line Disallowed';
  cmsgCommandLineDisallowed =
    'This Command Line has been set to be DISALLOWED (Terminal Only). If you really want to run this command you need '
    + 'to change that setting or run it in a Terminal.'
    + LineEnding + LineEnding
    ;

  ccapRequiredReading = 'REQUIRED READING';
  cmsgRequiredReading =
    'This Command Line has been set to display its notes for you to read and requires approval. '
    + LineEnding + LineEnding
    + 'Please read the notes below and then choose NO if you DO NOT WANT to continue: '
    + LineEnding + LineEnding
    + '==== ATTENTION ==='
    + LineEnding + LineEnding
    + '%s'
    + LineEnding + LineEnding
    ;

  cmsgOutputEndIndicator =
    LineEnding + LineEnding
    + '=============================================================='
    + LineEnding
    + '< END Command run ============================================'
    + LineEnding
    + '=============================================================='
    + LineEnding + LineEnding
    + LineEnding + LineEnding
    + LineEnding + LineEnding
    ;

  cmsgCommandsSaved = 'Saved by user at %s...'
                             + LineEnding + LineEnding;
  cmsgDisplayOutputCleared = 'Display Output Cleared by user at %s...'
                             + LineEnding + LineEnding;
  cmsgDisplayOutputTrimmed = '>>> >>> Display Output trimmed to about > %d < characters %s (set in OPTIONS)...'
                             + LineEnding + LineEnding;

  cmsgUpdated_DBFiles = '%s automatically updated to %s, Item %d';
  ccapUpdated_DB = 'Database';
  ccapUpdated_Prog = 'Program settings ';

  cmsgNoVersionFlagSpecified = 'Command "%s" has no %s flag specified.';

  cSearchHintGeneralCmd =
           'This presents search results for the various '
           + LineEnding
           + 'Favorites, KeyWords, and General Searches. '
           + LineEnding + LineEnding
           + 'The list can consist of both Commands '
           + LineEnding
           + '(listed first if any) and Command Lines '
           + LineEnding
           + '(listed second). If so there will be a '
           + LineEnding
           + '"separator" section between them. '
           + LineEnding + LineEnding
           + 'Clicking on an item shows it''s details to '
           + LineEnding
           + 'the right. If the selected item is a Command '
           + LineEnding
           + 'it''s Command Lines will be listed in '
           + LineEnding
           + 'the listbox below. '
           + LineEnding + LineEnding
           + 'Keep in mind that Commands / Command '
           + LineEnding
           + 'Lines that are in an edited state (i.e., not '
           + LineEnding
           + 'saved) may NOT or will NOT be included in the '
           + LineEnding
           + 'filtering. To see an accurate list be sure '
           + LineEnding
           + 'everything is saved. '
           + LineEnding + LineEnding
           + 'For the ClipBoard: '
           + LineEnding + LineEnding
           + 'Ctrl-Shift-C copies the entire list. '
           + LineEnding + LineEnding
           + 'Ctrl-f opens the find dialog. '
           + LineEnding + LineEnding
           + 'ctrl-g goes to the Cmd '
           + LineEnding + LineEnding;

  cSearchHintFavorites =
           'The Favorites list is, by default, automatically '
           + LineEnding
           + 'updated when you save after editing. When updated '
           + LineEnding
           + 'all previous selections are reset to point to the '
           + LineEnding
           + 'first item in all listboxes. If you don''t want this '
           + LineEnding
           + 'you can change in OPTIONS to disable this. In that case '
           + LineEnding
           + 'you will need to manually refresh the Favorites list. '
           + LineEnding + LineEnding;

  cSearchHintEnding = '<end>';

  cSearchHintGeneralCmdLine =
           'This will show any Command Lines belonging '
           + LineEnding
           + 'to the item selected in the list above if '
           + LineEnding
           + 'it is a Command. '
           + LineEnding + LineEnding
           + 'Click on a Command Line to enable the '
           + LineEnding
           + 'RUN button. '
           + LineEnding + LineEnding
           + 'It will be automatically cleared in some cases '
           + LineEnding
           + 'like when you edit the Master Keywords '
           + LineEnding
           + 'or maybe in some other cases. '
           + LineEnding + LineEnding
           + '<Enter> runs the command line. '
           + LineEnding + LineEnding
           + 'For the ClipBoard: '
           + LineEnding + LineEnding
           + 'Ctrl-c copies the selected command Line '
           + LineEnding
           + 'Ctrl-Shift-C copies the entire list '
           + LineEnding + LineEnding
           + 'Ctrl-f opens the find dialog. '
           + LineEnding + LineEnding
           + 'ctrl-g goes to the Command Line '
           + LineEnding + LineEnding;

   cSearchHintSearch =
           'These are global GENERAL SEARCH: '
           + LineEnding
           + '<ctrl-1> Search '
           + LineEnding
           + '<ctrl-2> New Search '
           + LineEnding
           + '<ctrl-3> Load Search '
           + LineEnding + LineEnding
           + 'Local to this page: '
           + LineEnding
           + '<ctrl-4> save search '
           + LineEnding
           + '<ctrl-5> display search '
           + LineEnding + LineEnding
           + '<end> ';

   cSearchHintKeyWords =
           'These are global KEYWORD SEARCH: '
           + LineEnding
           + '<alt-1> Search Keywords '
           + LineEnding
           + '<alt-2> New Search KeyWords '
           + LineEnding
           + '<alt-3> Load Search '
           + LineEnding + LineEnding
           + 'Local to this page: '
           + LineEnding
           + '<alt-4> save search '
           + LineEnding
           + '<alt-5> display search '
           + LineEnding + LineEnding
           + '<end> ';


   ccapRevert = 'Revert to DB data';
   cmsgRevert = 'This will revert all changes back to orginal values. Do you still want to Revert??';
   ccapUnsavedData = 'Unsaved Data!';
   cmsgUnsavedData = 'Either Editing or Unsaved Data, Do you still want to close??';
   cmsgFailedSave = 'Failure during save: %s';
   cmsgSQLiteLibNotFound = 'sqlite system library "%s" not found';
   ccapNothingLoaded = 'Nothing Loaded';
   cmsgNothingLoaded = 'There is nothing loaded, this Database is empty.';
   cmsgBadData_ESC = 'Output bad: %s which are not allowed in %s because of possible '
                     + 'insecure consequences. Run this command in a Terminal (can be changed in OPTIONS).';
   cmsgBadData_ESC_BASE = 'Output contains ESCAPE characters';
   cmsgBadData_ESC_OK = LineEnding + LineEnding +
                        '%s. This can mess up the output display. By default the output is thrown out. This can be changed in OPTIONS. '
                        + LineEnding + LineEnding;
   ccapDBMergeInfo = 'Merging TO sql DB''s:';
   cmsgDBMergeInfo =
     'Depending on the amount of data you are merging, and how much of that data already exists in the '
     + 'Target DB, and the speed of your hard drives, the process of merging to a sql DB can take time. '
     + LineEnding + LineEnding
     + 'Please be patient, it will finish. '
     + LineEnding + LineEnding
     + 'According to the hompage of the sqlite3 team, speed is limited because DB transactions '
     + 'must be ensured to be accurate and definitely written to disk for the safey of your data.';

   cmsgDispNoNotes = '< No Notes >';
   cmsgDispNoKeyWords = '< No KeyWords >';

   ccapShowUnsavedMessage = 'Search Scope';
   cmsgShowUnsavedMessage =
     'Searches only use the SAVED database.'
     + Lineending + LineEnding
     + 'Currently you have unsaved data, so your search '
     + 'may not be complete until you have saved all '
     + 'the edited entries.'
     ;

   cmsgEmptySearchResults = '< No Search Results >';

   ccapHaltProcess = 'Halt detached process';
   cmsgHaltProcess = 'This will halt the selected process. Careful. The process may need '
     + 'to save data, best to find the program and close it normally. Do you still want to halt it??';

   ccapSaveFileExists = 'Saving to an existing file';
   cmsgSaveFileExists = 'The file "%s" already exists. Do you want to over-write it?';

   cmsgNoPathInsertion = 'The Command "%s" is a "%s", it has no path.';

   cSaveToFileMsg = 'message';
   cSaveToFileAbout = 'about';
   cSaveToFileIntro = 'introduction';
   cSaveToFileTips = 'tips';
   cSaveToFileFreshStart = 'freshstart';
   cSaveToFileUpgrade = 'upgrade';
   cSaveToFileIbus = 'ibus';
   cSaveToFileOutput = 'OutPut_%s';

   cOutPutCopy1 = 'Copy';
   cOutPutSave1 = 'Save';
   cOutPutCopy2 = 'the clipboard';
   cOutPutSave2 = 'a file';
   cOutPutCopy3 = 'copied';
   cOutPutSave3 = 'saved';

   cOutPutActionHint =
     '%s the Output area to %s.'
     + LineEnding + LineEnding
     + 'If nothing is selected the entire output will '
     + LineEnding
     + 'be %s, otherwise only the selected '
     + LineEnding
     + 'portion.'
     + LineEnding + LineEnding
     + '<end>';

   ccapOptSqliteSearch = 'Attempt to re-set sqlib';
   cmsgOptSqliteSearch =
     'commandoo could not find the sqlite 3 library, is the sqllite 3 library package installed? '
     + 'If you are sure that it is, maybe it is in an unusual location and you need to find the library. '
     + 'Once found add it manually using the button to the left.'
     + LineEnding + LineEnding
     + 'I can run the command "locate -i -e libsqlite3" for you which should give the location of the '
     + '"libsqlite3.so.0" file. Would you like me to do that for you?'
     + LineEnding
     ;

   cOutPutChangedBaseFolder =
     '==> Program re-start needed'
     + LineEnding + LineEnding
     + 'You changed the base folder path, commandoo needs to be restarted before the changes can take effect. '
     + 'If you have other copies open best to close them all.';

implementation

end.

