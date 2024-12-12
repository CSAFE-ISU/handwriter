# The handwriter R package performs writership analysis of handwritten
# documents. Copyright (C) 2021 Iowa State University of Science and Technology
# on behalf of its Center for Statistics and Applications in Forensic Evidence
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https://www.gnu.org/licenses/>.


# The following variables are loaded from the data folder so we
# can't fix the note 'no visible binding for global variable' by adding
# variable <- NULL at the beginning of the function that uses the variable.
# Instead, we declare the variables here to fix the note.
utils::globalVariables(c("templateK40"))
