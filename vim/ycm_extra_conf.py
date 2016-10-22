import os
import os.path
import logging
import ycm_core

BASE_FLAGS_CC = [
    '-Weverything', '-Wno-c++11-extensions', '-Wno-c++98-compat', '-Wno-padded',
    '-std=c++14', '-x', 'c++', '-I/usr/include/', '-I/usr/include/c++/6.2.1/'
]
SOURCE_EXTENSIONS = ['.cpp', '.cxx', '.cc', '.c', '.m', '.mm']
HEADER_EXTENSIONS = ['.h', '.hxx', '.hpp', '.hh']


def IsHeaderFile(filename):
    extension = os.path.splitext(filename)[1]
    return extension in HEADER_EXTENSIONS


def GetCompilationInfoForFile(database, filename):
    if IsHeaderFile(filename):
        basename = os.path.splitext(filename)[0]
        for extension in SOURCE_EXTENSIONS:
            replacement_file = basename + extension
            if os.path.exists(replacement_file):
                compilation_info = database.GetCompilationInfoForFile(
                    replacement_file)
                if compilation_info.compiler_flags_:
                    return compilation_info
        return None
    return database.GetCompilationInfoForFile(filename)


def FindProjectRoot(path):
    files = os.listdir(path)
    root_marks = ['cmakelists', 'tags', 'makefile', 'readme', '.git',
                  '.gitignore', 'cmake', 'license', 'src', 'include']
    root_flags = set()
    for file in files:
        filename = os.path.splitext(file)[0].lower()
        if filename in root_marks:
            root_flags.add(filename)

    logging.info(root_flags)
    if len(root_flags) >= 3:
        logging.info("Found project root: " + path)
        return path
    else:
        parent = os.path.dirname(os.path.abspath(path))
        if (parent == path):
            raise RuntimeError("Could not find project root")
        return FindProjectRoot(parent)


def FlagsForInclude(root):
    try:
        include_path = os.path.join(root, 'include')
        flags = ["-I" + include_path]
        #  for dirroot, dirnames, filenames in os.walk(include_path):
        #  for dir_path in dirnames:
        #  real_path = os.path.join(dirroot, dir_path)
        #  flags = flags + ["-I" + real_path]
        return flags
    except:
        return None


def MakeRelativePathsInFlagsAbsolute(flags, working_directory):
    if not working_directory:
        return list(flags)
    new_flags = []
    make_next_absolute = False
    path_flags = ['-isystem', '-I', '-iquote', '--sysroot=']
    for flag in flags:
        new_flag = flag

        if make_next_absolute:
            make_next_absolute = False
            if not flag.startswith('/'):
                new_flag = os.path.join(working_directory, flag)

        for path_flag in path_flags:
            if flag == path_flag:
                make_next_absolute = True
                break

            if flag.startswith(path_flag):
                path = flag[len(path_flag):]
                new_flag = path_flag + os.path.join(working_directory, path)
                break

        if new_flag:
            new_flags.append(new_flag)
    return new_flags


def FlagsForCompilationDatabase(root, filename):
    try:
        compilation_db_path = os.path.join(root, 'compile_commands.json')
        compilation_db_dir = os.path.dirname(compilation_db_path)
        logging.info("Set compilation database directory to " +
                     compilation_db_dir)
        compilation_db = ycm_core.CompilationDatabase(compilation_db_dir)

        if not compilation_db:
            logging.info("Compilation database file found but unable to load")
            return None

        compilation_info = GetCompilationInfoForFile(compilation_db, filename)

        if not compilation_info:
            logging.info("No compilation info for " + filename +
                         " in compilation database")
            return None

        logging.info("Load compilation info for " + filename)

        return MakeRelativePathsInFlagsAbsolute(
            compilation_info.compiler_flags_,
            compilation_info.compiler_working_dir_)
    except:
        logging.info("Load compilation info error")
        return None


def FlagsForFile(filename):
    root = FindProjectRoot(os.path.dirname(filename))
    compilation_db_flags = FlagsForCompilationDatabase(root, filename)
    basename = os.path.splitext(filename)[1]
    final_flags = BASE_FLAGS_CC

    if compilation_db_flags:
        logging.info("Load compilation_db for" + filename)
        final_flags += compilation_db_flags
    else:
        if IsHeaderFile(filename):
            final_flags += FlagsForInclude(root)

    logging.info(final_flags)

    return {'flags': final_flags, 'do_cache': True}
