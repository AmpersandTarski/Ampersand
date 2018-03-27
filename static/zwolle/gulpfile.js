var gulp = require('gulp')
var concat = require('gulp-concat')
var sourcemaps = require('gulp-sourcemaps')
var uglify = require('gulp-uglify-es').default
var ngAnnotate = require('gulp-ng-annotate')
var minifycss = require('gulp-minify-css')
var rename = require('gulp-rename')
var filter = require('gulp-filter')
var templateCache = require('gulp-angular-templatecache')
var addStream = require('add-stream')
var mainBowerFiles = require('gulp-main-bower-files')
var flatten = require('gulp-flatten')
var clean = require('gulp-clean')
const jsValidate = require('gulp-jsvalidate')

function prepareTemplates(folder) {
    return gulp.src(folder + '**/*.html')
        //.pipe(minify and preprocess the template html here)
        .pipe(templateCache('templates.js', { root: folder, module: 'AmpersandApp' }));
}

/**
 * Gather external library js/css/fonts into dist/lib folder
 */
gulp.task('build-lib', function (done) {
    var filterJS = filter('**/*.js', { restore: true })
    var filterCSS = filter('**/*.css', { restore: true })
    var filterFonts = filter('**/fonts/*.*', { restore: true })

    gulp.src('bower.json') // point to bower.json
        // https://github.com/mauricedb/gulp-main-bower-files
        .pipe(mainBowerFiles({
            overrides: {
                bootstrap: {
                    main: [
                        './dist/js/bootstrap.js',
                        './dist/css/bootstrap.min.css',
                        './dist/fonts/*.*'
                    ]
                }
            }
        }))
        // library javascript
        .pipe(filterJS)
        .pipe(concat('lib.min.js'))
        .pipe(uglify())
        .pipe(gulp.dest('app/dist/lib'))
        .pipe(filterJS.restore)
        // library css
        .pipe(filterCSS)
        .pipe(concat('lib.min.css'))
        .pipe(minifycss())
        .pipe(gulp.dest('app/dist/lib'))
        .pipe(filterCSS.restore)
        // library fonts
        .pipe(filterFonts)
        .pipe(flatten())
        .pipe(gulp.dest('app/dist/fonts'))
        .pipe(filterFonts.restore)
    done()
})

/**
 * Gather ampersand js/css/html into dist folder
 */
gulp.task('build-ampersand', function (done) {
    // js
    gulp.src(['app/src/module.js', 'app/src/**/*.js'])
        .pipe(addStream.obj(prepareTemplates('app/src/')))
        .pipe(sourcemaps.init())
        .pipe(concat('ampersand.js'))
        .pipe(jsValidate())
        .pipe(ngAnnotate())
        .pipe(sourcemaps.write('.'))
        .pipe(gulp.dest('app/dist'))
        .pipe(filter('**/*.js')) // only .js files go through
        .pipe(rename({ suffix: '.min' }))
        .pipe(uglify())
        .pipe(sourcemaps.write('.'))
        .pipe(gulp.dest('app/dist'))
    // css
    gulp.src(['app/src/module.css', 'app/src/**/*.css'])
        .pipe(concat('ampersand.css'))
        .pipe(gulp.dest('app/dist'))
        .pipe(rename({ suffix: '.min' }))
        .pipe(minifycss())
        .pipe(gulp.dest('app/dist'))
    done()
})

/**
 * Gulp function to build the application after Ampersand has generated the prototype
 */
gulp.task('build-project', function (done) {
    // css
    gulp.src(['app/project/**/*.css', 'extensions/**/*.css', '!extensions/**/lib/**/*'])
        .pipe(concat('project.css'))
        .pipe(gulp.dest('app/dist'))
        .pipe(rename({ suffix: '.min' }))
        .pipe(minifycss())
        .pipe(gulp.dest('app/dist'))
    // js
    gulp.src(['app/project/**/*.js', 'extensions/**/*.js', '!extensions/**/lib/**/*'])
        .pipe(addStream.obj(prepareTemplates('app/project/')))
        .pipe(sourcemaps.init())
        .pipe(concat('project.js'))
        .pipe(jsValidate())
        .pipe(ngAnnotate())
        .pipe(sourcemaps.write('.'))
        .pipe(gulp.dest('app/dist'))
        .pipe(filter('**/*.js')) // only .js files go through
        .pipe(rename({ suffix: '.min' }))
        .pipe(uglify())
        .pipe(sourcemaps.write('.'))
        .pipe(gulp.dest('app/dist'))
    done()
})

gulp.task('clean', function (done) {
    gulp.src('app/dist', { read: false, allowEmpty: true })
        .pipe(clean())
    done()
})

gulp.task('dist', gulp.series('clean', 'build-lib', 'build-ampersand'))