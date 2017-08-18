;;;; glt.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:glt)

;; This project is a simple "modern" OpenGL template that can be
;; copied and modified for future projects.  The design is intentionally simple,
;; but hopefully "full featured" enough to not need significant redesign for
;; each new project.

;; This project is based in part on the code from https://open.gl/drawing but 
;; translated to Common Lisp, and re-designed to not be a giant 'main' function.

;; Simple vertex shader expecting position, normal, and color in a vertex array
(defparameter *vertex-shader-text*
"
#version 330 core

in vec3 position;
in vec3 normal;
in vec4 color;

out vec4 Color;

uniform float time;

void main()
{

    float scale_factor = 0.25;
    float pi = 3.141592654;
    float x_angle = 0.0;
    float y_angle = 0.0;
    float z_angle = time;

    const int num_lights = 3;
    vec3[num_lights] light_positions;
    light_positions[0] = vec3(10.0, 0.0, 0.0);
    light_positions[1] = vec3(0.0, 10.0, 0.0);
    light_positions[2] = vec3(0.0, 0.0, 10.0);

    vec4[num_lights] light_colors;
    light_colors[0] = vec4(1.0, 0.0, 0.0, 1.0);
    light_colors[1] = vec4(0.0, 1.0, 0.0, 1.0);
    light_colors[2] = vec4(0.0, 0.0, 1.0, 1.0);

    mat4 translate = mat4(1.0, 0.0, 0.0, 0.0,
                      0.0, 1.0, 0.0, 0.0,
                      0.0, 0.0, 1.0, 0.0,
                      0.0, 0.0, 0.0, 1.0);

    mat4 x_rotate = mat4(1.0, 0.0, 0.0, 0.0,
                         0.0, cos(x_angle), -sin(x_angle), 0.0,
                         0.0, sin(x_angle), cos(x_angle), 0.0,
                         0.0, 0.0, 0.0, 1.0);
    mat4 y_rotate = mat4(cos(y_angle), 0.0, sin(y_angle), 0.0,
                     0.0, 1.0, 0.0, 0.0,
                     -sin(y_angle), 0.0, cos(y_angle), 0.0,
                     0.0, 0.0, 0.0, 1.0);

    mat4 z_rotate = mat4(cos(z_angle), -sin(z_angle), 0.0, 0.0,
                     sin(z_angle), cos(z_angle), 0.0, 0.0,
                     0.0, 0.0, 1.0, 0.0,
                     0.0, 0.0, 0.0, 1.0);

    mat4 scale = mat4(scale_factor, 0.0, 0.0, 0.0,
                  0.0, scale_factor, 0.0, 0.0,
                  0.0, 0.0, scale_factor, 0.0,
                  0.0, 0.0, 0.0, scale_factor);

    mat4 xform =  z_rotate * y_rotate * x_rotate * translate;
    vec4 xformed_normal = transpose(inverse(xform)) * vec4(normalize(normal), 1.0);
//    vec4 color = xformed_normal;

    //float u = position.x + time
    //float v = position.y + time
    //gl_Position = vec4(sin(u) * sin(v), cos(u) * sin(v), cos(v), 1.0)

    vec4 point = xform * vec4(scale_factor*position, 1.0);
    vec4 ncolor = vec4(0,0,0, 1.0);

    for (int i=0; i < num_lights; ++i)
    {
        vec4 L = normalize(point - xform * vec4(light_positions[i], 1.0));
        ncolor += max(0,dot(L, xformed_normal))* light_colors[i];
    }
    Color = ncolor * color;
    gl_Position = point;
}
")

;; Simple fragment shader that simply assigns the input color to the output color
(defparameter *fragment-shader-text*
"
#version 330 core

in vec4 Color;
out vec4 outColor;

void main()
{
    outColor = Color;
}
")

(defgeneric cleanup (obj))

(defstruct shader-input
  "Describes how a shader input value is stored in the vertex array."
  (name "" :type string)
  (count 3 :type fixnum)
  (type :float)
  (stride 0)
  (offset 0)
  (attrib 0))

(defstruct shader
  "A shader object."
  (type :fragment-shader)
  (text "" :type string)
  (shader 0 :type fixnum)
  (inputs nil :type (or list null))
  (outputs nil :type (or list null)))

(defun init-shader (sdr)
  "Load and compile a shader and print any compiler messages."
  (with-slots (text type shader) sdr
    (setf shader (gl:create-shader type))
    (gl:shader-source shader (list text))
    (gl:compile-shader shader)
    (when (not (eq t (gl:get-shader shader :compile-status)))
      (format t "compile-status: ~a~%" (gl:get-shader shader :compile-status))
      (format t "info-log ~a~%" (gl:get-shader-info-log shader)))))
      
(defmethod cleanup ((obj shader))
  "Delete a shader on the GPU."
  (with-slots (shader) obj
    (gl:delete-shader shader)))

(defstruct gl-object
  "An OpenGL object."
  (vao 0 :type fixnum)
  (vbo 0 :type fixnum)
  (ebo 0 :type fixnum)
  (count 0 :type fixnum)
  (shaders nil)
  (shader-program nil)
  (pos-attrib 0))

(defun to-gl-float-array (arr)
  "Create an OpenGL float array from a CL array of numbers.
   This is a convenience function that will coerce array elments to single-float."
  (declare (optimize (speed 3)))
  (let* ((count (length arr))
         (gl-array (gl:alloc-gl-array :float count)))
    (dotimes (i count)
      (setf (gl:glaref gl-array i) (coerce (aref arr i) 'single-float)))
    gl-array))

(defun to-gl-array (arr type)
  "Create an OpenGL array of the specified type, initialized with the contents of arr."
  (declare (optimize (speed 3)))
  (let* ((count (length arr))
         (gl-array (gl:alloc-gl-array type count)))
    (dotimes (i count)
      (setf (gl:glaref gl-array i) (aref arr i)))
    gl-array))

(defun create-gl-object (in-vertices in-indices in-shaders)
  "Create a vertex array object and initialize it on the GPU."
  (let* ((buffers (gl:gen-buffers 2))
         (gl-object (make-gl-object :vao (gl:gen-vertex-array)
                                      :vbo (car buffers)
                                      :ebo (cadr buffers)
                                      :count (length in-indices)
                                      :shaders in-shaders
                                      :shader-program (gl:create-program)))

         (gl-vertices (to-gl-float-array in-vertices))
         (gl-indices (to-gl-array in-indices :unsigned-int)))

    (with-slots (vao vbo ebo shaders shader-program) gl-object
      (gl:bind-vertex-array vao)

      (dolist (shader shaders)
        (init-shader shader))

      (gl:bind-buffer :array-buffer vbo)
      (gl:buffer-data :array-buffer :static-draw gl-vertices)
      (gl:free-gl-array gl-vertices)

      (gl:bind-buffer :element-array-buffer ebo)
      (gl:buffer-data :element-array-buffer :static-draw gl-indices)
      (gl:free-gl-array gl-indices)

      (dolist (shader shaders)
        (gl:attach-shader shader-program (shader-shader shader))
        (loop
           for output in (shader-outputs shader)
           for idx from 0
           do (gl:bind-frag-data-location shader-program idx output)))

      (gl:link-program shader-program)
      (let ((status (gl:get-program shader-program :link-status)))
        (format t "link-program: ~a~%~a~%" status(gl:get-program-info-log shader-program)))

      (gl:validate-program shader-program)
      (let ((status (gl:get-program shader-program :validate-status)))
        (format t "validate-program: ~a~%~a~%" status (gl:get-program-info-log shader-program)))

      (gl:use-program shader-program)

      (dolist (shader shaders)
        (loop
           for input in (shader-inputs shader)
           for idx from 0
           do (with-slots (name count type stride offset attrib) input
                (format t "~a~%" name)
                (setf attrib (gl:get-attrib-location shader-program name))
                (gl:enable-vertex-attrib-array attrib)
                (gl:vertex-attrib-pointer attrib count type :FALSE stride offset)))))

      (gl:bind-vertex-array 0)
      gl-object))


(defmethod cleanup ((obj gl-object))
  "Free up OpenGL resources created for a gl-object."
  (with-slots (vao vbo ebo shaders shader-program) obj
    (gl:delete-buffers (list vbo))
    (gl:delete-vertex-arrays (list vao))
    (setf vbo 0)
    (setf vao 0)
    (setf ebo 0)
    (gl:delete-program shader-program)
    (dolist (shader shaders)
      (cleanup shader))))

(defun render (object)
  "Render a gl-object."
  (with-slots (vao ebo count shaders shader-program) object
    (gl:uniformf (gl:get-uniform-location shader-program "time") (glfw:get-time))
    (gl:enable :line-smooth :polygon-smooth
               :depth-test :depth-clamp)
    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:clear :color-buffer :depth-buffer)
    (gl:use-program shader-program)
    (gl:bind-vertex-array vao)
    (%gl:draw-elements :triangles count :unsigned-int ebo)
    (gl:bind-vertex-array 0)))

(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close)))

(defun r-random (&key (min 0.0f0) (max 1.0f0))
  (+ min (random (- max min))))

(defun real-main (fname)
  (with-init
    (let* ((monitor (glfw:get-primary-monitor))
           (cur-mode (glfw:get-video-mode monitor))
           (cur-width (getf cur-mode '%cl-glfw3:width))
           (cur-height (getf cur-mode '%cl-glfw3:height))
           (stl-data (stl:read-stl fname))

           (float-size   (cffi:foreign-type-size :float))
           (stride       (* (+ 3 3 4) float-size))
           (vert-offset  (* 0 float-size))
           (norm-offset  (* 3 float-size))
           (color-offset (* 6 float-size))

           (vertex-inputs (list 
                           (make-shader-input :name "position"
                                              :type :float
                                              :count 3
                                              :stride stride
                                              :offset vert-offset)
                           (make-shader-input :name "normal"
                                              :type :float
                                              :count 3
                                              :stride stride
                                              :offset norm-offset)
                           (make-shader-input :name "color"
                                              :type :float
                                              :count 4
                                              :stride stride
                                              :offset color-offset)
                           ))

           ;; Create the vertex and fragment shaders
           (vert-shader (make-shader :text *vertex-shader-text*
                                     :type :vertex-shader
                                     :inputs vertex-inputs
                                     :outputs nil))

           (frag-shader (make-shader :text *fragment-shader-text*
                                     :type :fragment-shader
                                     :inputs nil
                                     :outputs (list "outColor"))))
      (with-window (:title "OpenGL Shader Test"
                           :width cur-width
                           :height cur-height
                           :decorated nil
                           :monitor monitor
                           :opengl-profile :opengl-core-profile
                           :context-version-major 3
                           :context-version-minor 3
                           :opengl-forward-compat t
                           :resizable nil)

        (setf %gl:*gl-get-proc-address* #'get-proc-address)
        (set-key-callback 'quit-on-escape)
        (gl:clear-color 0 0 0 0)

        (multiple-value-bind (vertices indices) (stl:to-opengl stl-data :red 1.0 :green 1.0 :blue 1.0 :alpha 1.0)
          (let ((scene (create-gl-object vertices indices (list vert-shader frag-shader))))
            ;; The 'event loop' 
            (loop until (window-should-close-p)
               do (render scene)
               do (swap-buffers)
               do (poll-events))

            ;; Finally clean up
            (cleanup scene)))))))

(defun main (fname &optional (in-thread #+os-macosx t))
  ;; OSX requires that GUI interaction is done in the main thread.
  ;; On other platforms, it's easier to debug in Emacs/Slime when
  ;; everything stays in the Slime interaction thread.
  (if in-thread
      (trivial-main-thread:with-body-in-main-thread ()
        (real-main fname))
      (real-main fname)))

