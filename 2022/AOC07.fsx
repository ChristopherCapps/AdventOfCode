let input = System.IO.File.ReadLines "/Users/chris/Development/fsharp/AdventOfCode/2022/07.input" 


type CdDirectory = Root | Parent | Subdirectory of string
type Command = Cd of CdDirectory | Ls
type FileSystemItem = File of string * int | Directory of string
type Line = Command of Command | FileSystemItem of FileSystemItem

type FileSystem = Map<string, FileSystemItem list> 

let parseCommand (line: string) =
    if (line.StartsWith("$ cd ")) then
        match line.[5..] with
        | "/" -> Cd(Root)
        | ".." -> Cd(Parent)
        | name -> Cd(Subdirectory name)
    else Ls

let parseFileSystemItem (line: string) =
    if (line.StartsWith("dir")) then
        Directory (line.[4..])
    else
        let [|sizeStr; name|] = line.Split([|' '|])
        File(name, int sizeStr)

let parseLine (line: string) =
    if (line.StartsWith('$')) then 
        Line.Command (parseCommand line)
    else 
        Line.FileSystemItem (parseFileSystemItem line)

type Path = Path of string

let RootDir = Path "/"

let parentDirOf (Path currentDir) =
    let idx = currentDir.LastIndexOf('/')
    if (idx = 0 || idx = -1) then RootDir else (Path (currentDir.Substring(0, idx)))

let subDirOf (Path currentDir) subdir =
    if (currentDir = "/") then 
        (Path (currentDir + subdir))
    else 
        (Path (currentDir + "/" + subdir))

let addFileSystemItem (fs: FileSystem) (Path currentDir) (fsi: FileSystemItem) =
    let dirContents = 
        match fs.TryFind currentDir with
        | Some contents -> contents
        | None -> []
    fs |> Map.add currentDir (dirContents @ [fsi])

let buildFileSystem (fs: FileSystem, currentDir: Path) (line: Line) =
    match line with
    | Command cmd ->
        match cmd with
        | Cd cdDirectory -> 
            match cdDirectory with
            | Root -> (fs, RootDir)
            | Parent -> (fs, parentDirOf currentDir)
            | Subdirectory subdir -> (fs, subDirOf currentDir subdir)
        | Ls -> (fs, currentDir)
    | FileSystemItem fsi -> (addFileSystemItem fs currentDir fsi, currentDir)

let fs = 
    input
    |> Seq.map parseLine
    |> Seq.fold buildFileSystem (Map.empty<string, FileSystemItem list>, RootDir)
    |> fst

let rec dirSize (fs: FileSystem) (Path name) =
    fs.[name]
    |> Seq.sumBy 
        (fun fsi -> 
            match fsi with
            | File (_, length) -> length
            | Directory subdir -> dirSize fs (subDirOf (Path name) subdir))

let totalSizeOfDirsUnder100000 =
    fs.Keys
    |> Seq.map (Path >> (dirSize fs))
    |> Seq.filter (fun size -> size <= 100000)
    |> Seq.sum

// Part 2

let unusedSpace = 70000000 - (dirSize fs RootDir)

let largestDirectoryToDeleteSize =
    fs.Keys
    |> Seq.map (Path >> (dirSize fs))
    |> Seq.filter (fun size -> size <= unusedSpace)
    |> Seq.max
