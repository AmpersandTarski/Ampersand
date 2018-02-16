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

function prepareTemplates() {
    return gulp.src('app/src/**/*.html')
        //.pipe(minify and preprocess the template html here)
        .pipe(templateCache('templates.js', { root: 'app/src/', module: 'AmpersandApp' }));
}

// https://github.com/mauricedb/gulp-main-bower-files
gulp.task('libjs', function (done) {
    var filterJS = filter('**/*.js', { restore: true })
    var filterCSS = filter('**/*.css', { restore: true })
    var filterFonts = filter('**/fonts/*.*', { restore: true })

    gulp.src('./bower.json') // point to bower.json
        .pipe(mainBowerFiles({
            overrides: {
                bootstrap: {
                    main: [
                        './dist/js/bootstrap.js',
                        './dist/css/*.min.css',
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

gulp.task('js', function (done) {
    gulp.src(['app/src/module.js', 'app/src/**/*.js', 'app/project/**/*.js'])
        .pipe(addStream.obj(prepareTemplates()))
        .pipe(sourcemaps.init())
        .pipe(concat('ampersand.js'))
        .pipe(ngAnnotate())
        .pipe(sourcemaps.write('.'))
        .pipe(gulp.dest('app/dist'))
        .pipe(filter('**/*.js')) // only .js files go through
        .pipe(rename({ suffix: '.min' }))
        .pipe(uglify())
        .on('error', function (err) {
            console.error(err.toString());
        })
        .pipe(sourcemaps.write('.'))
        .pipe(gulp.dest('app/dist'))
    done()
})

gulp.task('css', function (done) {
    gulp.src(['app/src/module.css', 'app/src/**/*.css', 'app/project/**/*.css'])
        .pipe(concat('ampersand.css'))
        .pipe(gulp.dest('app/dist'))
        .pipe(rename({ suffix: '.min' }))
        .pipe(minifycss())
        .pipe(gulp.dest('app/dist'))
    done()
})

gulp.task('watch', function (done) {
    gulp.watch(['app/src/**/*.js', 'app/js/**/*.js'], ['js'])
    done()
})

gulp.task('clean', function (done) {
    gulp.src('app/dist', { read: false, allowEmpty: true })
        .pipe(clean())
    done()
})

gulp.task('default', gulp.series('clean', 'css', 'js', 'libjs'))