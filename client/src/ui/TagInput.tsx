import React, { useState } from 'react'
import styled from 'styled-components'
import { colors } from './color'
import { Input, useInput } from './Input'
import { Icons } from './Icon'

interface TagInputProps extends React.InputHTMLAttributes<HTMLInputElement> {
  tags: string[]
  onAddTag: (tag: string) => void
  onRemoveTag: (tag: string) => void
}

const TagContent = styled.div`
  display: inline-block;
  color: ${colors.white};
  background-color: ${colors.blue200};
  padding: 4px;
  border-radius: 2px 0 0 2px;
`

const TagsContainer = styled.div`
  display: flex;
  padding-bottom: 10px;
`

const TagContainer = styled.div`
  display: flex;
`

const TagRemoveButton = styled.button`
  background-color: ${colors.blue300};
  border: 0;
  cursor: pointer;
  margin-right: 7px;
  height: 26px;
  color: ${colors.white};
  border-radius: 0 2px 2px 0;
  &:focus {
    outline: none;
  }
`

export const TagInput: React.FC<TagInputProps> = ({
  tags,
  placeholder,
  onRemoveTag,
  onAddTag
}) => {
  const input = useInput({
    trim: true,
    validations: [],
    shouldShowError: _ => false
  })

  return (
    <div>
      <Input
        {...input}
        placeholder={placeholder}
        onKeyDown={e => {
          if (e.key === 'Enter') {
            onAddTag(input.value)
            input.setValue('')
          }
        }}
      />
      {tags.length > 0 && (
        <TagsContainer>
          {tags.map(tag => (
            <TagContainer>
              <TagContent>{tag}</TagContent>
              <TagRemoveButton onClick={() => onRemoveTag(tag)}>
                <Icons.times />
              </TagRemoveButton>
            </TagContainer>
          ))}
        </TagsContainer>
      )}
    </div>
  )
}

export const useTagInput = () => {
  const [tags, setTags] = useState<string[]>([])

  return {
    tags,
    onAddTag: (tag: string) => {
      setTags([...new Set([...tags, tag])])
    },
    onRemoveTag: (tag: string) => setTags(tags.filter(x => x !== tag))
  }
}
