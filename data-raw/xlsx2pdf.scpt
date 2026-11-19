-- Exports the active sheet of each workbook to PDF with Microsoft Excel.
-- Usage: osascript data-raw/xlsx2pdf.scpt in1.xlsx out1.pdf [in2.xlsx out2.pdf ...]
-- Excel is sandboxed: keep the files in ~/Library/Containers/com.microsoft.Excel/Data
-- so it can read and write them without asking for access.
on run argv
  tell application "Microsoft Excel"
    set display alerts to false
    repeat with i from 1 to (count of argv) by 2
      open workbook workbook file name (item i of argv)
      set wb to active workbook
      save as active sheet filename (item (i + 1) of argv) file format PDF file format
      close wb saving no
    end repeat
  end tell
end run
