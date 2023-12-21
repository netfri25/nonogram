#define NOB_IMPLEMENTATION
#include "nob.h"

#include <stdlib.h>

#define CC     "gcc"
#define CFLAGS "-pedantic", "-Wall", "-Wextra", "-Wswitch-enum"
#define LIBS   "-lraylib", "-lm"

#define SRC_DIR     "src"
#define BUILD_DIR   "build"

#define PROG_NAME   "main"

void compiler_flags(Nob_Cmd* const cmd) {
    char const* const output_path = BUILD_DIR "/" PROG_NAME;
    nob_cmd_append(cmd, CC, CFLAGS, "-o", output_path);
}

bool files_to_compile(char const* const src_dir, Nob_File_Paths* const files) {
    bool result = true;

    Nob_File_Paths file_paths = {0};
    if (!nob_read_entire_dir(src_dir, &file_paths))
        nob_return_defer(false);

    for (size_t i = 0; i < file_paths.count; i++) {
        char const* const file_name = file_paths.items[i];
        if (file_name[0] == '.') continue;

        Nob_String_View file_name_sv = nob_sv_from_cstr(file_name);
        Nob_String_View file_ext = {
            .data = file_name_sv.data + file_name_sv.count - 2,
            .count = 2,
        };

        // nob_log(NOB_INFO, "FOUND: %s/%s", src_dir, file_name);

        if (nob_sv_eq(file_ext, nob_sv_from_parts(".c", 2))) {
            file_name_sv.count -= 2; // remove the file extension
            nob_da_append(files, nob_temp_sv_to_cstr(file_name_sv));
        }
    }

defer:
    if (file_paths.items) nob_da_free(file_paths);
    return result;
}

Nob_Procs compile_to_objects(Nob_File_Paths const* const files) {
    Nob_String_Builder output_path_sb = {0};
    nob_sb_append_cstr(&output_path_sb, BUILD_DIR "/");
    size_t const output_path_sb_count = output_path_sb.count;

    Nob_String_Builder file_path_sb = {0};
    nob_sb_append_cstr(&file_path_sb, SRC_DIR "/");
    size_t const file_path_sb_count = file_path_sb.count;

    Nob_Procs procs = {0};
    for (size_t i = 0; i < files->count; i++) {
        output_path_sb.count = output_path_sb_count;
        file_path_sb.count = file_path_sb_count;

        char const* const file_path = files->items[i];

        // construct the input path string
        nob_sb_append_cstr(&file_path_sb, file_path);
        nob_sb_append_cstr(&file_path_sb, ".c");
        nob_sb_append_null(&file_path_sb);

        // construct the output path string
        nob_sb_append_cstr(&output_path_sb, file_path);
        nob_sb_append_cstr(&output_path_sb, ".o");
        nob_sb_append_null(&output_path_sb);

        if (nob_file_exists(output_path_sb.items)) {
            int res = is_path1_modified_after_path2(file_path_sb.items, output_path_sb.items);
            if (res == 0) continue;
        }

        // compile to an object file
        Nob_Cmd cmd = {0};
        nob_cmd_append(&cmd, CC, CFLAGS);
        nob_cmd_append(&cmd, "-c", nob_temp_strdup(file_path_sb.items));
        nob_cmd_append(&cmd, "-o", nob_temp_strdup(output_path_sb.items));

        Nob_Proc pid = nob_cmd_run_async(cmd);
        nob_da_append(&procs, pid);

        nob_cmd_free(cmd);
    }

    if (output_path_sb.items) nob_sb_free(output_path_sb);
    if (file_path_sb.items) nob_sb_free(file_path_sb);
    return procs;
}

void libs_to_link(Nob_Cmd* const cmd) {
    nob_cmd_append(cmd, LIBS);
}

bool files_to_link(char const* const build_dir, Nob_Cmd* const cmd) {
    bool result = true;

    Nob_File_Paths file_paths = {0};
    if (!nob_read_entire_dir(build_dir, &file_paths))
        nob_return_defer(false);

    Nob_String_Builder file_path_sb = {0};
    nob_sb_append_cstr(&file_path_sb, BUILD_DIR "/");
    size_t const file_path_sb_count = file_path_sb.count;

    for (size_t i = 0; i < file_paths.count; i++) {
        file_path_sb.count = file_path_sb_count;
        char const* const file_name = file_paths.items[i];
        if (file_name[0] == '.') continue;

        nob_sb_append_cstr(&file_path_sb, file_name);
        nob_sb_append_null(&file_path_sb);

        Nob_String_View file_name_sv = nob_sv_from_cstr(file_name);
        Nob_String_View file_ext = {
            .data = file_name_sv.data + file_name_sv.count - 2,
            .count = 2,
        };

        // nob_log(NOB_INFO, "FOUND: %s/%s", build_dir, file_name);

        if (nob_sv_eq(file_ext, nob_sv_from_parts(".o", 2))) {
            nob_cmd_append(cmd, nob_temp_strdup(file_path_sb.items));
        }
    }

defer:
    if (file_paths.items) nob_da_free(file_paths);
    return result;
}

int main(int const argc, char const* const* const argv) {
    NOB_GO_REBUILD_URSELF(argc, argv);
    int result = EXIT_SUCCESS;

    Nob_File_Paths files = {0};
    if (!files_to_compile(SRC_DIR, &files))
        nob_return_defer(EXIT_FAILURE);

    nob_mkdir_if_not_exists(BUILD_DIR);
    Nob_Procs procs = compile_to_objects(&files);
    if (procs.count == 0 && nob_file_exists(BUILD_DIR "/" PROG_NAME))
        nob_return_defer(EXIT_SUCCESS);

    if (!nob_procs_wait(procs))
        nob_return_defer(EXIT_FAILURE);

    Nob_Cmd cmd = {0};
    compiler_flags(&cmd);
    files_to_link(BUILD_DIR, &cmd);
    libs_to_link(&cmd);

    if (!nob_cmd_run_sync(cmd))
        nob_return_defer(EXIT_FAILURE);

defer:
    if (cmd.items)   nob_cmd_free(cmd);
    if (procs.items) nob_da_free(procs);
    if (files.items) nob_da_free(files);
    return result;
}
