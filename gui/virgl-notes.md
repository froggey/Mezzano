Misc notes
=========

On start, context 0 and subcontext 0 are always available. No need to create a context.

Creating a new context will automatically create a subcontext 0, don't need to do that.

Interesting GL initial state after subcontext creation:
* far val = 1.0 forall viewports
* vertex array bound unless has\_feature(feat\_gles31\_vertex\_attrib\_binding)
* sub->fb\_id = framebuffer x1
* sub->blit\_fb\_ids = framebuffer x2

Actually that's not very interesting. the GL state is effectively 0, everything must be configured.

Shaders are in TGSI text format, must be nul-terminated strings.

Large shader sources can be sent over multiple create shader commands.

SO fields are only processed in the initial create shader command, not in the continuations.

Resources are global.

Resources must be attached to contexts (not subcontexts) before they can be used. (virtio-gpu-attach-resource)

Resources are distinct from objects.

Objects are per-subcontext

don't clear all color buffers, just the ones you're using. (for speed, not correctness)

resources must still be flushed to update the scanout

virtio-gpu-cmd-resource-create-2d creates a resource with the PIPE\_TEXTURE\_2D target and the BIND\_RENDER\_TARGET binding. 0 samples, 0 last level.
they seems to work ok as a framebuffer target.

If you send an invalid command and virglrenderer reports a context error ("context error reported [...]"), then that context is irrevocably screwed. it will not do any 3d stuff. destroy it, create a new one (hope you weren't using context 0) and start over.

You pick context, object, and resource ids.

Shaders are passed in the TGSI text format, but create\_shader also wants to know how log it will be in the binary format.

Some VIRGL\_CCMDs have subcommands based on the object type field.
* `VIRGL\_CCMD\_BIND\_OBJECT`
* `VIRGL\_CCMD\_CREATE\_OBJECT`

Where do uniforms come from?
Constant buffers.
How are constant buffers bound?
VIRGL\_CCMD\_SET\_UNIFORM\_BUFFER. duh.

How are vertex & fragment shaders linked together?
Shader input & output attachments must match up.

Context 0 has access to all resources when transfer-to/from-host-2d/3d is
called, not just attached resources.

A minimal and incomplete introduction to TGSI
==========
Shaders start with a header 'VERT' or 'FRAG' or one of the other kinds.

Followed by input and output declarations:
*  declaration = 'DCL' register [',' attachment] [',' interpolation]
*  register = ('IN'|'OUT') '[' index ']'
*  attachment = 'POSITION'|'COLOR'|('GENERIC' '[' index ']')
*  interpolation = 'CONSTANT'|'LINEAR'|'PERSPECTIVE'|'COLOR'

Followed by immediate definitions:
*  immediate = 'IMM' 'FLT32' '{' float ',' float ',' float ',' float '}'

Followed by instructions, terminated by 'END'

There are no comments. This is very incomplete, there are more kinds of declarations & immediates.

Vertex shader input declarations are mapped to the vertex element thing elements, `virgl\_create/bind\_vertex\_elements`

Vertex shader outputs get mapped to fragment shader inputs. The POSITION attachment must be specified for one of them. I don't know how GENERIC attachments work yet.

Fragment shader input attachments should match the vertex shader output attachments, register indices don't seem to matter.

Fragment shader should have a "DCL OUT[0], COLOR" output, this is used as the fragment colour output. I think.

Instructions consist of an opcode followed by the single destination and an arbitrary number of sources.

Immediate values are referenced using the "IMM[n]" register, when n is nth immediate definition.


Actually doing stuff
=======
```
// Detect displays
scanout, width, height = virtio\_gpu\_get\_display\_info(...)

// Create a resource to render to, attach guest backing memory, and set it as the scanout.
render\_id = 42
format = virgl\_format\_b8g8r8a8\_unorm // or whichever you want.
virtio\_gpu\_resource\_create\_2d(render\_id, width, height, format); // should use the 3d call, but it doesn't seem matter for render targets.
// actually the 2d call assumes context=0, so don't use it if that matters.

// Supports scatter-gather, so can be noncontigious in guest physical memory.
// The guest portion does not need to cover the entire resource! Uploads can be done bit by bit.
virtio\_gpu\_resource\_attach\_backing(render\_id, ...);
// Can change x/y to move the scanout around the resource (scrolling, page-flip, whatever)
// Changing width/height does not change the display size. The output will be
// scaled to match the physical display.
virtio\_gpu\_set\_scanout(scanout, render\_id, x=0, y=0, width, height)

// Drawing with the CPU, like some kind of savage:
// Update an x,y,w,h box in the resource, pulling data from the offset in the attachment.
// not sure how the stride works out here. might be w\*bpp. the supplied width, not the width of the resource.
virtio\_gpu\_transfer\_to\_host\_2d(x,y,w,h, guest\_attachment\_offset, render\_id)
// Flushes an x,y,w,h box in the resource to any scanouts it's attached to.
virtio\_gpu\_resource\_flush(x,y,w,h, render\_id);

// 3d setup
// Note: virgl\_foo functions send a virgl command using virtio\_gpu\_submit\_3d
// and are assumed to be operating on my\_context.

// Create a new context (very important, hosing context 0 is annoying)
my\_context = 123
virtio\_gpu\_ctx\_create(my\_context)
// Attach the framebuffer resource to the context.
virtio\_gpu\_attach\_resource(render\_id, my\_context);
// Create a surface backed by it.
// I don't know what the layers or levels are. I've left them at zero for now.
render\_surface\_id = 219
virgl\_create\_surface(render\_id, render\_surface\_id, format, 0, 0);
// Set the render resource as the framebuffer color0 surface.
// No zsurface configured yet.
virgl\_set\_framebuffer\_state(zsurf=0, csurfs=[render\_surface\_id])
// At this point you can do clears.

// Clear:
virgl\_clear(PIPE\_CLEAR\_COLOR0, red, green, blue, alpha, depth, stencil)

// Rendering stuff.
// Create vertex buffer & fill it. This is a resource.
// width is size in bytes. to hold float4 position & float4 colour * 3 vertices
virtio\_gpu\_resource\_create\_3d(vertex\_buffer, width=32*3, height=1, depth=1, bind=VIRGL\_BIND\_VERTEX\_BUFFER, target=PIPE\_BUFFER, format=0, array-size=1, last-level=0, nr-samples=0, flags=0);
// Attach to context
virtio\_gpu\_attach\_resource(vertex\_buffer, my\_context);
// Attach backing memory & upload vertices.
virtio\_gpu\_resource\_attach\_backing(vertex\_id, ...);
virtio\_gpu\_transfer\_to\_host\_3d(my\_context, vertex\_id, x=0, y=0, z=0, w=size, h=1, d=1, offset=0, level=0, stride=0, layer-size=0);
// Dump it in the vbo thing at index 0.
vertex\_buffer\_vbo\_index = 0
virgl\_set\_vertex\_buffers([(stride=32, // 2*float4
                              offset=0,
                              handle=vertex\_buffer)])
// Create vertex elements array, describes the layout of vertex arrays.
// One vertex buffer shared by two vertex arrays
// First buffer is the position array, second is the colour array.
virgl\_create\_vertex\_elements(vertex\_elements, [(src\_offset=0,
                                                    instance\_divisor=0,
                                                    vertex\_buffer\_index=vertex\_buffer\_vbo\_index,
                                                    src\_format=VIRGL\_FORMAT\_R32G32B32A32\_FLOAT),
                                                   (src\_offset=16,
                                                    instance\_divisor=0,
                                                    vertex\_buffer\_index=vertex\_buffer\_vbo\_index,
                                                    src\_format=VIRGL\_FORMAT\_R32G32B32A32\_FLOAT)])
// Create blend state, colormask starts off cleared.
// Assumes virgl\_create\_blend does not use independent-blend.
virgl\_create\_blend(blend\_handle,
                     logicop\_enable=false,
                     logicop=false,
                     dither=false,
                     alpha\_to\_coverage=false,
                     alpha\_to\_one=false,
                     blend\_enable=false,
                     rgb\_func=0,
                     rgb\_src\_factor=0,
                     rgb\_dst\_factor=0,
                     alpha\_func=0,
                     alpha\_src\_factor=0,
                     alpha\_dst\_factor=0,
                     colormask=PIPE\_MASK\_RGBA);
// Create shaders. No stream output or stride parts.
virgl\_create\_shader(vertex\_shader, "VERT
DCL IN[0]
DCL IN[1]
DCL OUT[0], POSITION
DCL OUT[1], COLOR
MOV OUT[0], IN[0]
MOV OUT[1], IN[1]
END")
virgl\_create\_shader(fragment\_shader, "FRAG
DCL IN[0], COLOR, COLOR
DCL OUT[0], COLOR
IMM FLT32 { 1.0, 1.0, 1.0, 1.0 }
SUB OUT[0], IMM[0], IN[0]
END")
// Bind blend state.
virgl\_bind\_blend(blend\_handle);
// Bind current VBO with vrend\_bind\_vertex\_elements\_state
virgl\_bind\_vertex\_elements(vertex\_elements)
// Bind shaders.
virgl\_bind\_shader(vertex\_shader, PIPE\_SHADER\_VERTEX);
virgl\_bind\_shader(fragment\_shader, PIPE\_SHADER\_FRAGMENT);
// Mode is PIPE\_PRIM\_\*foo
virgl\_draw\_vbo(start, count, mode, indexed=false, instance-count=0, start-instance=0, primitive-restart=false, restart-index=0, index-bias=0, min-index=0, max-index=!0)

// Update the scanout and actually show things.
// Just like the 2D side.
virtio\_gpu\_resource\_flush(x,y,w,h, render\_id);
```

Fences
=====
Iunno.

Documentation
==========

virtio-gpu virtio device, 2d commands only:

https://www.kraxel.org/virtio/virtio-v1.0-cs03-virtio-gpu.pdf

All virtio-gpu command definitions:

https://github.com/qemu/qemu/blob/master/include/standard-headers/linux/virtio_gpu.h

Note: virtio\_gpu\_cmd\_submit is immediately followed by the command buffer.

Virgl commands:

https://github.com/freedesktop/virglrenderer/blob/master/src/virgl_protocol.h

Defines the layout of commands passed via submit\_3d

Virgl texture formats and virtio-gpu capset info:

https://github.com/freedesktop/virglrenderer/blob/master/src/virgl_hw.h

Gallium pipe defines:

https://github.com/freedesktop/virglrenderer/blob/master/src/gallium/include/pipe/p_defines.h

These are the PIPE_foo defines. Very important.

TGSI:

https://www.freedesktop.org/wiki/Software/gallium/tgsi-specification.pdf

https://gallium.readthedocs.io/en/latest/tgsi.html

An example shader: https://lists.freedesktop.org/archives/mesa-dev/2011-April/007056.html
