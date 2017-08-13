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

void main()
{
    Color = color;
    gl_Position = vec4(normal + position, 1.0);
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
      (gl:use-program shader-program)

      (dolist (shader shaders)
        (loop
           for input in (shader-inputs shader)
           for idx from 0
           do (with-slots (name count type stride offset attrib) input
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
    (gl:enable :line-smooth :polygon-smooth)
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

(defun real-main ()
  (with-init
    (let* ((monitor (glfw:get-primary-monitor))
           (cur-mode (glfw:get-video-mode monitor))
           (cur-width (getf cur-mode '%cl-glfw3:width))
           (cur-height (getf cur-mode '%cl-glfw3:height)))
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

        ;; This is still a bit ugly
        (let* (
               ;; Packed array:  position       normal         color 
               (vertices     #(-0.5  0.5 0.0  0.0 0.0 1.0  1.0 0.0 0.0 1.0
                               0.5  0.5 0.0  0.0 0.0 1.0  0.0 1.0 0.0 1.0
                               0.5 -0.5 0.0  0.0 0.0 1.0  0.0 0.0 1.0 1.0
                               -0.5 -0.5 0.0  0.0 0.0 1.0  0.5 0.0 0.5 1.0))

               (indices      #(0 1 2 2 3 0))

               ;; Describe how the vertex shader input data is packed in
               ;; the vertex array
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
                                                  :offset color-offset)))

               ;; Create the vertex and fragment shaders
               (vert-shader (make-shader :text *vertex-shader-text*
                                         :type :vertex-shader
                                         :inputs vertex-inputs
                                         :outputs nil))

               (frag-shader (make-shader :text *fragment-shader-text*
                                         :type :fragment-shader
                                         :inputs nil
                                         :outputs (list "outColor")))

               ;; Create and initialize an OpenGL opbject
               (scene (create-gl-object vertices indices (list vert-shader frag-shader))))

          ;; The 'event loop' 
          (loop until (window-should-close-p)
             do (render scene)
             do (swap-buffers)
             do (poll-events))

          ;; Finally clean up
          (cleanup scene))))))

(defun main (&optional (in-thread #+os-macosx t))
  ;; OSX requires that GUI interaction is done in the main thread.
  ;; On other platforms, it's easier to debug in Emacs/Slime when
  ;; everything stays in the Slime interaction thread.
  (if in-thread
      (trivial-main-thread:with-body-in-main-thread ()
        (real-main))
      (real-main)))

