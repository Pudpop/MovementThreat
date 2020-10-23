#requires -Version 5.0

# change $Path to a ZIP file that exists on your system!
$Path = "C:\Users\David\OneDrive\Documents\Work\Thesis\Code\Data\archive"

# change extension filter to a file extension that exists
# inside your ZIP file
$Filter = '*moving.csv'

$FilterTwo = '*.zip'

# change output path to a folder where you want the extracted
# files to appear
$OutPath = 'C:\Users\David\OneDrive\Documents\Work\Thesis\Code\temp'

# ensure the output folder exists
$exists = Test-Path -Path $OutPath
if ($exists -eq $false)
{
  $null = New-Item -Path $OutPath -ItemType Directory -Force
}

# load ZIP methods
Add-Type -AssemblyName System.IO.Compression.FileSystem

Get-ChildItem -Path $Path -Filter $FilterTwo |
    
    ForEach-Object{
        # open ZIP archive for reading
        $zip = [System.IO.Compression.ZipFile]::OpenRead($_.FullName)

        # find all files in ZIP that match the filter (i.e. file extension)
        $zip.Entries | 
          #Where-Object { $_.FullName -like $Filter } |
          ForEach-Object { 
            # extract the selected items from the ZIP archive
            # and copy them to the out folder
            $MatchName = $_.Name
            #[System.IO.Compression.ZipFileExtensions]::ExtractToFile($_, "$OutPath\$FileName", $true)
            $MatchName |
                ForEach-Object {
                    $RoundName = $_.Name
                }
            }

        # close ZIP file
        $zip.Dispose()

    }
    
# open out folder
explorer $OutPath 