# delphicomponents

RAD Studio Delphi Enhancing Components

During development of our product RAD & Installer (http://www.rad-installer.com) we needed to develop customized components to provide special functionality to our software.

These components are now available for everyone:

1. ProjectProperties - component (TPropertiesPage) with similar functionality as CPropertySheet known from MS Visual Studio (also known as tab dialog boxes: https://msdn.microsoft.com/en-us/library/d3fkt014.aspx)

TPropertiesPage supports multiple ways how the data can be presented: Checkbox, Edit, ComboBox:

2. NavigationBars - TEdit descendand (TAutoCompleteEdit) with fulltext search (auto completion) and bitmaps (owner draw) with suggestion list simulating TComboBox design.

It is based on ideas from here: http://stackoverflow.com/questions/33398089/delphi-tcombobox-with-fulltext-search-and-bitmap-owner-draw/33471653 and here: http://stackoverflow.com/questions/14283783/combobox-simple-with-bitmap with some additional functionality.

(There are currently 2 TAutoCompleteEdits on this picture (both with suggestions) - the component itself looks like TEdit, when you start typing or you click the down arrow suggestion list is shown like TComboBox.)

Full sources are included. You can download demo (Test Application) in Downloads.

3. Drag & Drop (TEdit) with coloring and Cue text.

During development of Graphical Installer for NSIS and Inno Setup (http://graphical-installer.com/) we decided to enhance and improve TDropFilesTarget component (http://flocke.vssd.de/prog/code/pascal/dft/) and add new features to it.

We added Success/Failed Coloring (Green/Red) and Cue text feature. If the component is draggable it is highlighted with light yellow color.


Drag & Drop also works in Win Vista/7/8/10 from UAC to non-UAC apps (see comments)

The included example shows how to use the non-visual component TDropFilesTarget with TEdit and other controls.

These projects are built with Embarcadero RAD Studio XE8 but there is no version specific code (you should be able to recompile the project in any RAD Studio version).

