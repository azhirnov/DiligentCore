project(spirv-reflect)

add_library(spirv-reflect STATIC
	spirv_reflect.c
	spirv_reflect.h)

target_include_directories(spirv-reflect
PUBLIC
	../SPIRV-Headers
	.
)
