var gulp = require('gulp')
var concat = require('gulp-concat')
var sourcemaps = require('gulp-sourcemaps')
var uglify = require('gulp-uglify-es').default
var ngAnnotate = require('gulp-ng-annotate')

gulp.task('js', function () {
  gulp.src(['app/AmpersandApp.js', 'app/src/**/*.js', 'app/js/*.js'])
    .pipe(sourcemaps.init())
      .pipe(concat('app.js'))
      .pipe(ngAnnotate())
      .pipe(uglify())
      .on('error', function(err){
          console.error(err.toString());
      })
    .pipe(sourcemaps.write())
    .pipe(gulp.dest('app/dist'))
})

gulp.task('watch', ['js'], function () {
  gulp.watch(['app/**/*.js', '!app/dist/*.js'], ['js'])
})