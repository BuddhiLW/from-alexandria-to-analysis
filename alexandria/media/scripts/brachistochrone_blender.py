"""Pure EDN-scene-graph-to-bpy emitter, run headless in Blender.

Input is JSON projection of alexandria.design/make-design; no path or shape is
hand-authored here. Coordinates use millimetres; Blender's gravity uses metres
per second squared after setting unit scale 0.001.

Regenerate the full watertight printable STL (28 MB, kept in target/, not git)
from the worktree root in two commands, one runtime at a time:
  cd alexandria && clojure -J-Xmx2g -Sdeps '{:deps {org.clojure/data.json {:mvn/version "2.5.2"}}}' -M -e '(require (quote alexandria.design) (quote clojure.data.json)) (spit "../target/design.json" (clojure.data.json/write-str (alexandria.design/make-design)))' && cd ..
  flatpak run --filesystem="$PWD" org.blender.Blender -b --factory-startup --python "$PWD/alexandria/media/scripts/brachistochrone_blender.py" -- "$PWD/target/design.json" "$PWD/target/design" --export
The --export flag voxel-unions and verifies the print mesh, and also exports
GLB; omit it to render only, add --animation for 72 PNG frames to encode with
ffmpeg -y -framerate 10 -i target/design/race-frame-%04d.png -c:v libx264 -crf 28 -preset slow -pix_fmt yuv420p target/design/brachistochrone-race.mp4.
"""
import bpy
import json
import math
import os
import sys
from mathutils import Matrix, Vector


def look_at(obj, target):
    """Point obj's -Z at target with world +Y (anti-gravity) as image up.

    Vector.to_track_quat('-Z', 'Y') aligns the local Y axis with world *Z*,
    which tilts gravity (world -Y) out of the image vertical and makes the
    descending tracks read as level. Build the basis explicitly instead.
    """
    back = (obj.location - target).normalized()
    right = Vector((0, 1, 0)).cross(back).normalized()
    up = back.cross(right)
    obj.rotation_euler = Matrix((right, up, back)).transposed().to_euler()


def material(name, color, metallic=0):
    m = bpy.data.materials.new(name)
    m.diffuse_color = (*color, 1)
    m.use_nodes = True
    bsdf = m.node_tree.nodes.get('Principled BSDF')
    bsdf.inputs['Base Color'].default_value = (*color, 1)
    bsdf.inputs['Metallic'].default_value = metallic
    bsdf.inputs['Roughness'].default_value = .32
    bsdf.inputs['Emission Color'].default_value = (*color, 1)
    bsdf.inputs['Emission Strength'].default_value = .42
    return m


def box(node, palette):
    o = node['opts']
    bpy.ops.mesh.primitive_cube_add(size=1, location=o['center'])
    obj = bpy.context.object
    obj.name = node['id']
    obj.dimensions = o['size']
    bpy.ops.object.transform_apply(location=False, rotation=False, scale=True)
    obj.data.materials.append(palette[o['material']])
    return obj


def tube(node, palette):
    """Closed manifold swept open-U trough. Eight cross-section vertices.

    The sample points represent marble centres. At each sample, the cross
    section wraps underneath the bead, with raised side lips in the z axis.
    The bevel is polygonal but water-tight; cross-sections share edges.
    """
    o = node['opts']
    points = [Vector(p) for p in o['points']]
    outer = o['outer-radius-mm']
    inner = o['inner-radius-mm']
    wall = o['wall-mm']
    # clockwise polygon, viewed along the track tangent
    profile = [(-outer, -inner-wall), (outer, -inner-wall),
               (outer, inner*.45), (inner, inner*.45),
               (inner, -inner), (-inner, -inner),
               (-inner, inner*.45), (-outer, inner*.45)]
    vertices = []
    for i, p in enumerate(points):
        tangent = (points[min(i+1, len(points)-1)] - points[max(i-1, 0)]).normalized()
        # normal perpendicular to the xy tangent, pointing approximately up
        normal = Vector((-tangent.y, tangent.x, 0))
        for z, h in profile:
            q = p + Vector((0, 0, z)) + normal*h
            vertices.append(tuple(q))
    count = len(profile)
    faces = []
    faces.append(tuple(reversed(range(count))))
    for i in range(len(points)-1):
        for j in range(count):
            a = i*count+j
            b = i*count+(j+1)%count
            faces.append((a, b, b+count, a+count))
    faces.append(tuple((len(points)-1)*count+j for j in range(count)))
    mesh = bpy.data.meshes.new(node['id']+'-mesh')
    mesh.from_pydata(vertices, [], faces)
    mesh.update()
    obj = bpy.data.objects.new(node['id'], mesh)
    bpy.context.collection.objects.link(obj)
    obj.data.materials.append(palette[o['material']])
    # Give the trough its own dark inner bed; the upper lip and side walls
    # remain bright and cast a thin contact shadow into the groove.
    inner = palette[o['material']+'-groove']
    obj.data.materials.append(inner)
    for face in mesh.polygons:
        if face.index > 0 and (face.index - 1) % count in (3, 4, 5):
            face.material_index = 1
    bevel = obj.modifiers.new('rounded channel lips', 'BEVEL')
    bevel.width = .55
    bevel.segments = 2
    bevel.affect = 'EDGES'
    bevel.limit_method = 'ANGLE'
    bevel.angle_limit = .75
    obj.modifiers.new('weighted normals', 'WEIGHTED_NORMAL')
    return obj


def sphere(node, palette):
    o = node['opts']
    bpy.ops.mesh.primitive_uv_sphere_add(segments=24, ring_count=12,
                                         radius=o['radius-mm'], location=o['center'])
    obj = bpy.context.object
    obj.name = node['id']
    obj.data.materials.append(palette[o['material']])
    return obj


def emit(design):
    bpy.ops.object.select_all(action='SELECT')
    bpy.ops.object.delete(use_global=False)
    scene = bpy.context.scene
    scene.unit_settings.system = 'METRIC'
    scene.unit_settings.scale_length = .001
    scene.gravity = (0, -9.81, 0)
    palette = {name: material(name, color, .78 if name == 'steel' else .16)
               for name, color in {'base': (.51,.56,.61), 'gate': (.98,.69,.19),
                                   'cycloid': (.95,.12,.09), 'arc': (.08,.43,.95),
                                   'line': (.25,.79,.51), 'steel': (.95,.97,1.0)}.items()}
    for name in ('cycloid', 'arc', 'line'):
        color = palette[name].diffuse_color
        palette[name+'-groove'] = material(name+'-groove', tuple(c*.30 for c in color[:3]))
    palette['steel'].node_tree.nodes.get('Principled BSDF').inputs['Roughness'].default_value = .12
    for name, color in {'cycloid': (1.0, .16, .12), 'arc': (.1, .55, 1.0),
                        'line': (.29, .96, .55)}.items():
        palette[name+'-bead'] = material(name+'-bead', color, .35)
        palette[name+'-bead'].node_tree.nodes.get('Principled BSDF').inputs['Roughness'].default_value = .12
    objects = {}
    for node in design['nodes']:
        kind = node['node']
        obj = {'box': box, 'tube-along-curve': tube, 'sphere': sphere}[kind](node, palette)
        if kind == 'sphere':
            obj.data.materials.clear()
            obj.data.materials.append(palette[node['id'].split('-')[0]+'-bead'])
            for face in obj.data.polygons:
                face.use_smooth = True
        objects[node['id']] = obj
    return objects


def print_stl(design, objects, path):
    printable = [objects[n['id']] for n in design['nodes'] if not n['opts'].get('simulation-only')]
    bpy.ops.object.select_all(action='DESELECT')
    for obj in printable:
        obj.select_set(True)
    bpy.context.view_layer.objects.active = printable[0]
    # A voxel union connects the base, all troughs and the catches. The
    # resulting STL has one watertight printable shell, not intersecting
    # manifold parts which would require slicer-side repair.
    bpy.ops.object.convert(target='MESH')
    bpy.ops.object.join()
    merged = bpy.context.object
    merged.name = 'printable-race'
    merged.data.remesh_voxel_size = .65
    bpy.ops.object.voxel_remesh()
    bpy.ops.wm.stl_export(filepath=path, export_selected_objects=True,
                          apply_modifiers=True)
    import bmesh
    bm = bmesh.new()
    bm.from_mesh(merged.data)
    boundary = sum(1 for edge in bm.edges if not edge.is_manifold)
    bm.free()
    print('STL', path, 'dimensions', list(merged.dimensions),
          'vertices', len(merged.data.vertices), 'nonmanifold_edges', boundary, flush=True)
    return merged


def camera_and_lights(dimensions, objects):
    w, h, d = dimensions
    scene = bpy.context.scene
    scene.world.use_nodes = True
    bg = scene.world.node_tree.nodes.get('Background')
    bg.inputs['Color'].default_value = (.19, .21, .24, 1)
    bg.inputs['Strength'].default_value = .28
    scene.render.film_transparent = False
    scene.view_settings.view_transform = 'AgX'
    scene.view_settings.look = 'AgX - Medium High Contrast'
    aim = Vector((w*.46, h*.45, 0))
    # Near-frontal view from +z, gravity (world -y) straight down the image:
    # all three profiles fall visibly from the shared start A (top left) to
    # the shared finish B (lower right). A small elevation and x offset
    # separate the green, blue and red lanes in depth.
    bpy.ops.object.camera_add(location=aim + Vector((w*.25, h*.35, d*3.0)))
    cam = bpy.context.object
    look_at(cam, aim)
    cam.data.type = 'ORTHO'
    scene.camera = cam
    # Project *actual* model bounds (including the finish tray) into camera
    # coordinates, rather than guessing the ortho scale from nominal size.
    bpy.context.view_layer.update()
    corners = [obj.matrix_world @ Vector(v) for obj in objects.values()
               for v in obj.bound_box]
    basis = cam.rotation_euler.to_matrix().inverted()
    projected = [basis @ (p-aim) for p in corners]
    lo = Vector((min(p.x for p in projected), min(p.y for p in projected), 0))
    hi = Vector((max(p.x for p in projected), max(p.y for p in projected), 0))
    center = (lo+hi)*.5
    cam.location += cam.rotation_euler.to_matrix() @ center
    aspect = scene.render.resolution_x / scene.render.resolution_y
    # Blender defines ortho_scale as the HORIZONTAL extent for a landscape
    # image: the visible height is scale / aspect. Fitting the height directly
    # to ortho_scale silently crops the model's top and finish tray.
    cam.data.ortho_scale = 1.22 * max(hi.x-lo.x, (hi.y-lo.y)*aspect)
    print('CAMERA_FIT', cam.data.ortho_scale, 'bounds', tuple(lo), tuple(hi), flush=True)
    # Matte dark studio sweep. A strong broad key and a smaller rim light
    # reveal rounded lips against the dark groove; ambient remains subdued.
    backdrop = material('charcoal-studio', (.105, .12, .14))
    backdrop.node_tree.nodes.get('Principled BSDF').inputs['Roughness'].default_value = .95
    backdrop.node_tree.nodes.get('Principled BSDF').inputs['Emission Strength'].default_value = .08
    bpy.ops.mesh.primitive_plane_add(size=1, location=(w*.48, h*.43, -d*1.8))
    sweep = bpy.context.object
    sweep.name = 'studio-backdrop-render-only'
    sweep.dimensions = (w*4, h*4, 0)
    sweep.data.materials.append(backdrop)
    for loc, power, size in [((w*.1, h*2.3, d*2.0), 2400, 190),
                             ((w*1.4, h*1.4, -d*1.3), 1000, 180),
                             ((w*.65, h*.4, d*2.0), 900, 160)]:
        bpy.ops.object.light_add(type='AREA', location=loc)
        light = bpy.context.object
        light.data.energy = power
        light.data.shape = 'DISK'
        light.data.size = size
        look_at(light, aim)


def race_caption(design):
    """Camera-locked timing legend in the lower letterbox, not printable geometry.

    These are the analytic raster times, not the independent Bullet trial.
    Keeping the label in camera coordinates ensures it remains legible when the
    model dimensions or the orthographic fit change.
    """
    cam = bpy.context.scene.camera
    scale = cam.data.ortho_scale
    aspect = bpy.context.scene.render.resolution_x / bpy.context.scene.render.resolution_y
    legend = 'FINISH  |  CYCLOID  %.3f s   |   ARC  %.3f s   |   STRAIGHT  %.3f s' % (
        design['times']['cycloid'], design['times']['arc'], design['times']['line'])
    curve = bpy.data.curves.new('race-times-render-only', type='FONT')
    curve.body = legend
    curve.size = scale * .019
    curve.align_x = 'CENTER'
    curve.align_y = 'CENTER'
    text = bpy.data.objects.new('race-times-render-only', curve)
    bpy.context.collection.objects.link(text)
    text.parent = cam
    text.location = (0, -scale / aspect * .475, -60)
    ink = material('caption-ivory', (.9, .94, 1.0))
    ink.node_tree.nodes.get('Principled BSDF').inputs['Emission Strength'].default_value = 1.0
    curve.materials.append(ink)
    print('CAPTION', legend, 'viewport-width', scale, flush=True)


def animate(design, objects):
    """Kinematic preview, parameterised by raster times; physically honest
    caveat: this is NOT a rigid-body finishing-order measurement. Separately
    install active rigid bodies for a simulated race in the .blend scene.
    """
    scene = bpy.context.scene
    scene.frame_end = 72
    scene.render.fps = 10
    for name in ['cycloid', 'arc', 'line']:
        marble = objects[name+'-marble']
        points = [Vector(p) for p in next(n['opts']['points'] for n in design['nodes'] if n['id']==name)]
        t = design['times'][name]
        for i in range(0, 73, 2):
            u = min(1., i/(t*10*3))
            idx = u*(len(points)-1)
            lo = min(int(idx), len(points)-2)
            marble.location = points[lo].lerp(points[lo+1], idx-lo)
            marble.keyframe_insert(data_path='location', frame=max(1,i))
        bpy.context.view_layer.objects.active = marble
        bpy.ops.rigidbody.object_add()
        marble.rigid_body.type = 'ACTIVE'
        marble.rigid_body.kinematic = True  # animated preview; simulation requires false
    print('RASTER_ORDER', design['finishing-order'], 'TIMES', design['times'], flush=True)


def simulate(design, objects, out):
    """Independent Bullet rigid-body trial, not the kinematic render.

    The actual completion order is measured at the common x finish plane,
    rather than inferred from raster times. Save evidence even on a mismatch.
    """
    scene = bpy.context.scene
    scene.frame_end = 360
    scene.render.fps = 60
    # The mathematical metre is rendered as 100 mm; preserve the descent
    # problem's time scale by using 100 * 9.81 mm/s² (not physical 1:1 size).
    scene.gravity = (0, -981, 0)
    for name in ['cycloid', 'arc', 'line']:
        track = objects[name]
        bpy.context.view_layer.objects.active = track
        bpy.ops.rigidbody.object_add()
        track.rigid_body.type = 'PASSIVE'
        track.rigid_body.collision_shape = 'MESH'
        track.rigid_body.friction = .2
        track.rigid_body.use_margin = True
        track.rigid_body.collision_margin = .05
        bead = objects[name+'-marble']
        bead.location.x += 2.5
        bpy.context.view_layer.objects.active = bead
        bpy.ops.rigidbody.object_add()
        bead.rigid_body.type = 'ACTIVE'
        bead.rigid_body.mass = .007
        bead.rigid_body.collision_shape = 'SPHERE'
        bead.rigid_body.friction = .2
        bead.rigid_body.restitution = 0
        bead.rigid_body.use_margin = True
        bead.rigid_body.collision_margin = .05
    scene.rigidbody_world.point_cache.frame_end = scene.frame_end
    finish = design['constraints']['span-mm']-6
    measured = {}
    last_positions = {}
    for frame in range(1, scene.frame_end+1):
        scene.frame_set(frame)
        for name in ['cycloid', 'arc', 'line']:
            if name not in measured:
                bead = objects[name+'-marble']
                pos = bead.evaluated_get(bpy.context.evaluated_depsgraph_get()).matrix_world.translation
                last_positions[name] = list(pos)
                if pos.x >= finish:
                    measured[name] = frame/scene.render.fps
        if len(measured) == 3:
            break
    order = sorted(measured, key=measured.get)
    result = {'raster_times': design['times'], 'raster_order': design['finishing-order'],
              'blender_finish_times': measured, 'blender_last_positions': last_positions,
              'blender_order': order,
              'matches_raster': order == design['finishing-order']}
    with open(os.path.join(out, 'race-grade.json'), 'w') as f:
        json.dump(result, f, indent=2)
    print('BLENDER_RIGID_BODY', result, flush=True)
    bpy.ops.wm.save_as_mainfile(filepath=os.path.join(out, 'brachistochrone-rigid.blend'))


def main():
    argv = sys.argv[sys.argv.index('--')+1:]
    source, out = argv[:2]
    os.makedirs(out, exist_ok=True)
    with open(source) as f:
        design = json.load(f)
    objects = emit(design)
    prefix = os.path.join(out, 'brachistochrone-race')
    if '--export' in argv:
        # Export before adding the render-only studio sweep and lights.
        bpy.ops.export_scene.gltf(filepath=prefix+'.glb', export_format='GLB',
                                  export_cameras=False, export_lights=False)
        print_stl(design, objects, prefix+'.stl')
        objects = emit(design)
    if '--simulate' in argv:
        simulate(design, objects, out)
        return
    scene = bpy.context.scene
    scene.render.engine = 'BLENDER_EEVEE'
    scene.render.resolution_x = 960
    scene.render.resolution_y = 540
    # A pair of contrasting stripes crosses each lane just before B.
    for lane in (-26, 0, 26):
        for j, color in enumerate(((.92, .78, .24), (.10, .12, .15))):
            stripe = material('finish-%s-%d' % (lane, j), color)
            bpy.ops.mesh.primitive_cube_add(size=1, location=(191+j*2.5, 35, lane))
            obj = bpy.context.object
            obj.name = 'finish-line-%s-%d-render-only' % (lane, j)
            obj.dimensions = (2.3, .42, 17)
            obj.data.materials.append(stripe)
    camera_and_lights(design['dimensions'], objects)
    race_caption(design)
    animate(design, objects)
    scene.render.resolution_percentage = 100
    scene.render.image_settings.file_format = 'PNG'
    scene.render.image_settings.color_mode = 'RGB'
    scene.frame_set(24)  # 2.3 s: cycloid at finish, arc nearly there
    scene.render.filepath = prefix+'.png'
    bpy.ops.render.render(write_still=True)
    bpy.ops.wm.save_as_mainfile(filepath=prefix+'.blend')
    if '--animation' in argv:
        scene.render.image_settings.file_format = 'PNG'
        scene.render.resolution_percentage = 100
        scene.frame_end = 72
        scene.render.filepath = os.path.join(out, 'race-frame-')
        bpy.ops.render.render(animation=True)
        print('ANIMATION_FRAMES', out, flush=True)
    print('ARTIFACTS', prefix, flush=True)


if __name__ == '__main__':
    main()
