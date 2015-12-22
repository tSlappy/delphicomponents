unit DropDownMember;

interface

uses
  Generics.Defaults, Generics.Collections, Classes,
  SysUtils;

type
    // Summary:
    //     Defines a span of text based on character and line index.
    TextSpan = record
        // Summary:
        //     Ending character index within the line. This value must be less than or equal
        //     to the length of the line.
        //[ComAliasName("Microsoft.VisualStudio.TextManager.Interop.CharIndex")]
        iEndIndex: Integer;
        //
        // Summary:
        //     Ending line index.
        iEndLine: Integer;
        //
        // Summary:
        //     Starting character index within the line. This value must be less than or
        //     equal to the length of line.
        //[ComAliasName("Microsoft.VisualStudio.TextManager.Interop.CharIndex")]
        iStartIndex: Integer;
        //
        // Summary:
        //     Starting line index.
        iStartLine: Integer;
    end;

    DROPDOWNFONTATTR = (
        // Summary:
        //     plain text
        FONTATTR_PLAIN = 0,
        //
        // Summary:
        //     bold text
        FONTATTR_BOLD = 1,
        //
        // Summary:
        //     italicized text
        FONTATTR_ITALIC = 2,
        //
        // Summary:
        //     underlined text
        FONTATTR_UNDERLINE = 4,
        //
        // Summary:
        //     grayed out text
        FONTATTR_GRAY = 8
    );

    TDropDownFontAttrSet = Set of DROPDOWNFONTATTR;

    DisplayTokenInSection = (ShowEveryWhere, ShowInCodeOnly, ShowInNotCode);

    ScriptScope = (
        /// <summary>
        /// Before [Code] line
        /// </summary>
        NoCode,
        /// <summary>
        /// After [Code] line - but not inside of LocalScope
        /// </summary>
        GlobalScope,
        /// <summary>
        /// Inside of Function or Procedure
        /// </summary>
        LocalScope);

    ObjectMemberType = (omtUnknown, omtConstructor, omtDestructor, omtFunction, omtProcedure, omtProperty);

type
    // From refactoring
    TDropDownMember = class
    private
      fSpan: TextSpan;

      public
        FontAttr: TDropDownFontAttrSet;
        Labell: String;
        Glyph: Integer;

        procedure SetSpan(span: TextSpan);
        property Span: TextSpan read fSpan write SetSpan;

        constructor Create(fon: TDropDownFontAttrSet; lab: String; gly: Integer);
    end;

implementation

procedure TDropDownMember.SetSpan(span: TextSpan);
begin
  Self.fSpan := span;
  Inc(Self.fSpan.iStartIndex);
  Inc(Self.fSpan.iEndIndex);
end;

constructor TDropDownMember.Create(fon: TDropDownFontAttrSet; lab: String; gly: Integer);
begin
  Self.FontAttr := fon;
  Self.Labell := lab;
  Self.Glyph := gly;
end;

end.
