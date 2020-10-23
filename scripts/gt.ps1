
$Src = 'C:\Users\David\Downloads\0-cyrus2017-csv'
$Dst = 'C:\Users\David\OneDrive\Documents\Work\Thesis\Code\Data\groundtruth'

$Extension = '*groundtruth.csv'

Write-Host 'Starting'

Get-ChildItem -Path $Src -Filter $Extension -Recurse |

    Where-Object {!$_.PsIsContainer} |

        ForEach-Object {


            if(Test-Path -Path (Join-Path -Path $Dst -ChildPath $_.Name))
            {

                $NameWithDirTag = (Split-Path -Path $_.FullName -NoQualifier)  -replace [regex]::Escape('\'), '-'

                $NewPath = Join-Path -Path $Dst -ChildPath $NameWithDirTag
            }

            else
            {
                $NewPath = $Dst
            }

		Write-Host 'Copying ' $_.FullName
            Copy-Item -Path $_.FullName -Destination $NewPath
        }